server {
  listen        *:80;
  server_name   s.vocabulink.com;
  access_log    /var/log/nginx/s.vocabulink.com.access_log main;
  error_log     /var/log/nginx/s.vocabulink.com.error_log info;
  root  /home/chris/project/vocabulink/s;
  expires 7d;
}
