server {
  listen        *:80;
  server_name   www.vocabulink.com;
  access_log    /var/log/nginx/www.vocabulink.com.access_log main;
  error_log     /var/log/nginx/www.vocabulink.com.error_log info;
  root  /home/chris/sites/vocabulink.com;

  location / {
    include     /etc/nginx/fastcgi_params;
    if ($request_method = POST) {
      fastcgi_pass      127.0.0.1:10033;
      break;
    }
    if ($is_args) {
      fastcgi_pass      127.0.0.1:10033;
      break;
    }
    if ($cookie_auth) {
      fastcgi_pass      127.0.0.1:10033;
      break;
    }
    if ($uri = '/member/login') {
      fastcgi_pass      127.0.0.1:10033;
      break;
    }
    default_type      "text/html; charset=utf-8";
    set $memcached_key "vocabulink.com:$request_uri";
    memcached_pass    127.0.0.1:11211;
    error_page        404 = /fastcgi;
  }

  location /fastcgi {
    include     /etc/nginx/fastcgi_params;
    fastcgi_pass        127.0.0.1:10033;
  }
}

server {
  listen        *:80;
  server_name   vocabulink.com;
  rewrite ^(.*) http://www.vocabulink.com$1 permanent;
}
