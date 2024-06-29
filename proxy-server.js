const http = require("http");
const https = require("https");

const server = http.createServer((req, res) => {
  const targetUrl = "https://boundvariable.space" + req.url;
  const parsedUrl = new URL(targetUrl);

  // Set CORS headers for all responses
  res.setHeader("Access-Control-Allow-Origin", "*");
  res.setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS");
  res.setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization");

  // Handle preflight requests
  if (req.method === "OPTIONS") {
    res.writeHead(204);
    res.end();
    return;
  }

  const options = {
    hostname: parsedUrl.hostname,
    port: 443,
    path: parsedUrl.pathname + parsedUrl.search,
    method: req.method,
    headers: {
      ...req.headers,
      host: parsedUrl.hostname,
    },
  };

  const proxyReq = https.request(options, (proxyRes) => {
    res.writeHead(proxyRes.statusCode, proxyRes.headers);
    proxyRes.pipe(res);
  });

  proxyReq.on("error", (e) => {
    console.error("Proxy request error:", e);
    res.writeHead(500);
    res.end("Proxy error: " + e.message);
  });

  if (req.method === "POST") {
    req.pipe(proxyReq);
  } else {
    proxyReq.end();
  }
});

const port = 3000;
server.listen(port, () => {
  console.log(`Proxy server running on http://localhost:${port}`);
});
