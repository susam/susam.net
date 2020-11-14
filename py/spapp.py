#!/usr/bin/env python3


import cgi
import urllib.parse
import datetime
import random

from makesite import fread, render


def app(environ, start_response):
    # Request parameters
    method = environ['REQUEST_METHOD']
    path = environ['PATH_INFO']
    query = {}
    form = {}

    if 'QUERY_STRING' in environ:
        for k, v in urllib.parse.parse_qsl(environ['QUERY_STRING']):
            query[k] = v

    if 'wsgi.input' in environ:
        fs = cgi.FieldStorage(fp=environ['wsgi.input'],
                              environ=environ,
                              keep_blank_values=True)
        for k in fs:
            form[k] = fs.getfirst(k)

    # Routes
    content = ''
    if method in ('HEAD', 'GET', 'POST'):
        if path == '/comment/':
            status = '200 OK'
            content = comment_form(environ, method, query, form)
        else:
            status = '404 Not Found'
            content = '<p>' + status + '</p>\n'
    else:
        status = '501 Not Implemented'
        content = '<p>' + status + '</p>\n'

    # Response
    headers = [
        ('Content-Type', 'text/html; charset=UTF-8'),
        ('Content-Length', str(len(content)))
    ]
    start_response(status, headers)
    return [content.encode()]


def comment_form(environ, method, query, form):
    error_lines = []
    success_lines = ['Comment was submitted successfully.',
                     'It may be published after review.']

    params = {
        'class': '',
        'name': '',
        'url': '',
        'comment': '',
        'notice': '',
    }


    # The id field (if any) in query parameters is used in the query
    # parameter of the POST URL and also in a hidden field.
    if method == 'GET':
        params['slug'] = query.get('slug', '').strip()

    elif method == 'POST':
        # During post submission, the id field in the query parameter
        # must match the hidden id field in the form submitted. Further
        # the email field should be empty.
        if ('slug' not in query or
            'slug' not in form or
            query['slug'].strip() == '' or
            form['slug'].strip() == '' or
            query['slug'] != form['slug'] or
            form['email'] != ''):
            # Report error.
            error_lines.append('Invalid request.')

        # Retrieve all form submission params.
        for key in ('slug', 'name', 'url', 'comment'):
            params[key] = form.get(key, '').strip()

        # Validate form params.
        if params['name'] == '':
            error_lines.append('You must mention your name.')

        # Validate comment.
        if params['comment'] == '':
            error_lines.append('You must write a comment message.')

        if error_lines:
            params['class'] = 'errors'
            notice_lines = error_lines
        else:
            params['class'] = 'success'
            notice_lines = success_lines
            write_comment(environ, params)

        notice_lines = ['<li>' + x + '</li>\n' for x in notice_lines]
        params['notice'] = '<ul>\n' + ''.join(notice_lines) + '</ul>\n'

    content = form_html(params)
    return content


def form_html(params):
    page_layout = fread('layout/page.html')
    form_layout = fread('layout/form.html')
    form_layout = render(page_layout, content=form_layout)
    params.update({
        'root': '../',
        'title': 'Post Comment',
        'subtitle': ' - Susam Pal',
        'current_year': datetime.datetime.now().year,
        'canonical_url': '/comment/',
        'imports':
            '<link rel="stylesheet" type="text/css" href="/css/form.css">',
    })
    content = render(form_layout, **params)
    return content


def write_comment(environ, params):
    data = []

    data.append('----- BEGIN METADATA -----')
    data.append('id: {}'.format(params['slug']))
    data.append('HTTP_USER_AGENT: ' + environ.get('HTTP_USER_AGENT', ''))
    data.append('REMOTE_ADDR: ' + environ.get('REMOTE_ADDR', 'None'))
    data.append('----- END METADATA -----')

    utc_date = datetime.datetime.now(datetime.timezone.utc)
    rand = random.randint(1, 1000000)

    data.append('<!-- date: {:%Y-%m-%d %H:%M:%S %z} -->'.format(utc_date))
    data.append('<!-- name: {} -->'.format(params['name']))
    if params['url'] != '':
        data.append('<!-- url: {} -->'.format(params['url']))
    data.append(params['comment'])
    data = '\n'.join(data) + '\n'

    filename = ('/opt/cache/' +
                'c_{}_{:%Y-%m-%d_%H-%M-%S}_{}.txt'.format(params['slug'],
                                                          utc_date, rand))

    with open(filename, 'w') as f:
        f.write(data)


if __name__ == '__main__':
    from wsgiref import simple_server
    server = simple_server.make_server('127.0.0.1', 8001, app)
    server.serve_forever()
