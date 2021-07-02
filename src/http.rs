use crate::{error::Result, jobs};
use std::collections::{BTreeMap, HashMap};
use std::io::Write;
use std::cell::RefCell;

// ----------------------------------------------------------------------------
// Interface

#[derive(Deserialize)]
struct RequestOptions {
    output_filename: Option<String>,
    body_filename: Option<String>,
}

#[derive(Serialize)]
struct Response<'a> {
    status_code: u16,
    headers: HashMap<&'a str, &'a str>,
    body: Option<&'a str>,
}

// If the response can be deserialized -> success.
// If the response can't be deserialized -> failure.
byond_fn! { http_request_blocking(method, url, body, headers, ...rest) {
    let req = match construct_request(method, url, body, headers, rest.first().map(|x| &**x)) {
        Ok(r) => r,
        Err(e) => return Some(e.to_string())
    };

    match submit_request(req) {
        Ok(r) => Some(r),
        Err(e) => Some(e.to_string())
    }
} }

// Returns new job-id.
byond_fn! { http_request_async(method, url, body, headers, ...rest) {
    let req = match construct_request(method, url, body, headers, rest.first().map(|x| &**x)) {
        Ok(r) => r,
        Err(e) => return Some(e.to_string())
    };

    Some(jobs::start(move || {
        match submit_request(req) {
            Ok(r) => r,
            Err(e) => e.to_string()
        }
    }))
} }

// If the response can be deserialized -> success.
// If the response can't be deserialized -> failure or WIP.
byond_fn! { http_check_request(id) {
    Some(jobs::check(id))
} }

// ----------------------------------------------------------------------------
// Shared HTTP client state

const VERSION: &str = env!("CARGO_PKG_VERSION");
const PKG_NAME: &str = env!("CARGO_PKG_NAME");

fn setup_http_client() -> reqwest::blocking::Client {
    use reqwest::{
        blocking::Client,
        header::{HeaderMap, USER_AGENT},
    };

    let mut headers = HeaderMap::new();
    headers.insert(
        USER_AGENT,
        format!("{}/{}", PKG_NAME, VERSION).parse().unwrap(),
    );

    Client::builder().default_headers(headers).build().unwrap()
}

thread_local! {
    static HTTP_CLIENT: RefCell<Option<reqwest::blocking::Client>> = RefCell::new(Some(setup_http_client()));
}

// ----------------------------------------------------------------------------
// Request construction and execution

struct RequestPrep {
    req: reqwest::blocking::RequestBuilder,
    output_filename: Option<String>,
}

fn construct_request(
    method: &str,
    url: &str,
    body: &str,
    headers: &str,
    options: Option<&str>,
) -> Result<RequestPrep> {
    HTTP_CLIENT.with(|cell| {
        let borrow = cell.borrow_mut();
        match &*borrow {
            Some(client) => {
                let mut req = match method {
                    "post" => client.post(url),
                    "put" => client.put(url),
                    "patch" => client.patch(url),
                    "delete" => client.delete(url),
                    "head" => client.head(url),
                    _ => client.get(url),
                };
        
                if !body.is_empty() {
                    req = req.body(body.to_owned());
                }
            
                if !headers.is_empty() {
                    let headers: BTreeMap<&str, &str> = serde_json::from_str(headers)?;
                    for (key, value) in headers {
                        req = req.header(key, value);
                    }
                }
            
                let mut output_filename = None;
                if let Some(options) = options {
                    let options: RequestOptions = serde_json::from_str(options)?;
                    output_filename = options.output_filename;
                    if let Some(fname) = options.body_filename {
                        req = req.body(std::fs::File::open(fname)?);
                    }
                }
            
                Ok(RequestPrep {
                    req,
                    output_filename,
                })
            }

            // If we got here we royally fucked up
            None => {
                let client = setup_http_client();
                let req = client.get("");
                let output_filename = None;
                Ok(RequestPrep {
                    req,
                    output_filename,
                })
            }

        }
    })
}

fn submit_request(prep: RequestPrep) -> Result<String> {
    let mut response = prep.req.send()?;

    let body;
    let mut resp = Response {
        status_code: response.status().as_u16(),
        headers: HashMap::new(),
        body: None,
    };

    let headers = response.headers().clone();
    for (key, value) in headers.iter() {
        if let Ok(value) = value.to_str() {
            resp.headers.insert(key.as_str(), value);
        }
    }

    if let Some(output_filename) = prep.output_filename {
        let mut writer = std::io::BufWriter::new(std::fs::File::create(&output_filename)?);
        std::io::copy(&mut response, &mut writer)?;
        writer.flush()?;
    } else {
        body = response.text()?;
        resp.body = Some(&body);
    }

    Ok(serde_json::to_string(&resp)?)
}

byond_fn! { start_http_client() {
    HTTP_CLIENT.with(|cell| {
        cell.replace(Some(setup_http_client()))
    });
    Some("")
} }

byond_fn! { shutdown_http_client() {
    HTTP_CLIENT.with(|cell| {
        cell.replace(None)
    });
    Some("")
} }
