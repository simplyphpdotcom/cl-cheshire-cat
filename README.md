# Summary

cl-chessire-cat is a project for a CL redirection server.

The core of the server is based on Edi Weitz (http://weitz.de/) CL librairies,
in particular:

 * Hunchentoot (Web Server)
 * CL-PPCRE

The persistence layer is ensured using CL-STORE
(http://common-lisp.net/project/cl-store).

# How to use

The recommended usage requires you to use SBCL (and optionnaly swank) and is
divided in a three component process:

 1. The `chessire.sh` script which is responsible for daemon management
 operations.
 2. The `chessire.lisp` script which is responsible for loading and
 starting the chessire daemon.
 3. The daemon itself.

You are encouraged to modify and adapt the first two component in the way most
convinient to your own use of the Chessire.

As mentionned, the default process is dependent on SBCL. However we would like
Chessire to be compatible with as many CL distributions as possible. Any
adaptation of the starting process be compatible with a different CL
distribution is welcome.

## chessire.sh

The daemon operations are performed using `scripts/chessire.sh`. The
first argument of the script is either `start`, `stop`, `restart` or `status`
and makes the script perform the appropriate action. The script should always be
executed as root.

The second argument is expected to be a configuration file. If none is given,
`/etc/chessire.conf` is the default configuration file.

chessire.sh is looking for a `pidfile=` entry in the configuration
file. If there is no such entry, `/var/run/chessire.pid` is used.

The required action is then performed by `chessire.sh`. If it needs to run the
server, it assumes that `scripts/chessire.lisp` is located as
`/usr/share/chessire/scripts/chessire.lisp` unless it finds a `system=`
directive in the configuration file.

## chessire.lisp

`chessire.lisp` is loading the configuration file and starting the
daemon. (Please refer to `config/chessire.conf` for the details of the
documentation). `chessire.lisp` expects the configuration file as its
first command-line argument and assumes that the cl-chessire-cat system is
loadable via `(asdf:load-system "cl-chessire-cat")`.

The starting process includes:

 1. starting to listen on the specified port
 2. daemon bookeeping (debugging, dropping privileges, swank loading)
 4. loading the redirection rules

The privileges are dropped after Chessire started listening. This is a
limitation from usocket and hunchentoot. This means:

 * you should not have any problem listening directly on the 80 port (or any
   other privileged port).
 * the few first requests could happen to be served as root. Since the
   redirection specifications is not loaded untill the privileges are dropped
   and this issue can be prevented by a system firewall (like iptables), we
   don't consider it a big security issue.

## Customization 

If the recommended usage does not fit your use of Chessire, feel free to adapt
any of the previously described steps. They are made to be highly and easily
customizable.

Note that the debugging bookeeping step in `chessire.lisp` is provided for
convinience. If you skip this part, Hunchentoot defaults will be used:

 * the debugger will never be called in case of an error
 * error pages will show complete error message and backtrace

# Behavior

The server is listening on the HTTP port (80), awaiting an HTTP query. Other
ports will work, but you must keep in mind that no port rewritting facility is
currently offered.

The server first search for a domain name rule to apply and then for a URI
rewritting rule. The details of each rule is descript is the following section.

The rules are always checked _in turn_, if a rule matches, it's applied (and the
search stop).

If no domain name matching rule is found and the domain name does not start with
`www.`, the server will send a "301 Moved Permanently" to the same URL with an
additional `www.` prefix to domain name. If the domain name already starts with
`www.`, the server will return a "404 Not Found" error in order to avoid
redirection loop.

If a domain name rule matched, Chessire then search its list of URI rule to
apply any addition URI modification. If there is no URI matching rule and no
domain rewritting in effect, the server will return a "404 Not Found" error in
order to prevent redirection loops.

Domain name rules parameters are used as default for its associated URI rules.

The loop protections intend to protect you from mild obvious rule specification
errors. Their goal is not to prevent willingful redirection loops or
mischievious configurations. For example it is easy to trick Chessire into
issuing infinite redirection loops with a domain name rule with no URI rule, key
`(:exact "www.domain.example")` and replacement `"www.domain.example"`. In other
words you are responsible for the correctness of your redirection
rules. Chessire will not check them for you and there is no plan to do so in the
future.

## Unspecified behavior

Since Chessire intends to be a production-grade product, we made our best to
keep unspecified behavior implementation on a fail-early and safe basis.

For example, if you provide an invalid argument for a rule specification, we try
to fail and send an error message when you create or update the rule rather than
at apply time.

However, this behavior may not be always easy to have, don't forget that
unspecified behaviour is still unspecified and may make Chessire to crash on
each and every request it receives.

# Rules

Each rule is composed of two main elements:

 * The rule's key is identifying whether the rule can be applied to a
   given URL. The key is itself composed of two elements:
  * The kind specifies the algorithm used to match the rule.
  * The match specification gives the information to match the rule against the
    URL.
 * The rule's effect including the modification to apply to the matching URL,
   options for the HTTP Answer or subsequent rules to apply.

## Domain rules

## Key

Three rules matching algorithms (kinds) are used:

 * `exact`
 * `suffix` (matches if the end of the domain name matches)
 * `regex`

Following the DNS specification
([[RFC1035](https://www.ietf.org/rfc/rfc1035.txt)]), domain name matches are
always case-insensitive.

## Effect

### Domain rewritting

If the rewrite specification is not nil, the domain will be rewritten.

If the rule is an `exact` match, the rewritte specification is use to replace the
whole domain name.

If the rule is a `suffix` match, the rewritte specification may contain once the
special substring ``"\`"``, which will be replaced by the prefix (non matching part)
of the original domain name.

If the rule is a `regex` match, any string replacement accepted by
`cl-ppcre:regex-replace` will be accepted and the result will be equivalent to
`(cl-ppcre:regex-replace regex original-domain-name replacement)`. Function
designators are not allowed since they cannot be saved easily.

## URI rules

A list of URI rewritting rules can be used for each domain.

### Key

Three rules matching algorithms (kinds) are used:

 * `exact`
 * `prefix` (matches if the beggining of the URI matches)
 * `regex`

URI match are case-sensitive by default (this can be changed via regex flags).

URI replacements for `exact` and `regex` match occurs the same way as Domain
rewritting.

If the rule is a `prefix` match, the rewritte specification may contain once the
special substring `"\'"`, which will be replaced by the suffix (non matching part)
of the original URI.

## Redirection parameters

Each redirection specification can also include redirection parameters.

Currently, the only parameter supported is `http-code` and its default value is
302.

`http-code` must be one of 300, 301, 302 (default), 303 or 307. This status code
will be the one used in the answer sent to the client. The behavior is
unspecified if an invalid status code specification is given.

# Management

Management options can be setup when creating the server.

Currently two management options are supported:

 * `admin-host`: The domain name used to manage the server (default is
 `"management.invalid"`). 
 * `admin-allowed`: A list of CIDR blocks. IP addresses in these blocks
 are allowed to manage the server (default is localhost only)

Each CIDR network specifications is a pair of two elements. The first one is an
IPv4 address (either a string in dotted notation `"127.0.0.1"` or a vector of
four integers in host order `#(127 0 0 1)`). The second is the prefix-length of
the CIDR block. If the second part is missing, its default value is 32.

The recommended tool to manage Chessire is [curl](curl.haxx.se) or another low
level HTTP or TCP tool such as nc(1) or telnet(1).

The management API is splited in three parts:

 1. Global management
 2. Domain rules management
 3. URI rules management

Each operation is specified using three mecanisms:

 1. The URI of the operation (to choose what you want to do).
 2. The GET parameters (URL query string) to select the rule you want to manage.
 3. The POST parameters to provide the information required by the operation you want to perform.

## Global management

Global management operations are impacting the behavior of the whole server.

Currently, Chessire supports only one global management operation: `/save-rules`

This operation accepts one optional POST parameter: `file` which is the
path to the file in which the rules will be stored. If the parameter is not
provided, the file used is the one specified for the `rules_file` configuration.

There is currently no check or security restriction on the path used the save
the files. It is your responsibility to ensure that the file is really the one
you want and to provide Chessire with an adequat privilege level.

Example:

```
POST /safe-rules HTTP/1.1
Host: management.invalid
Content-type: application/x-www-form-urlencoded

file=%2Fchessire-bkp%2Fmy-bkp.crr
```

## Domain name rule management

The domain rule management operations are in the "folder"
`/domain-name-rule/`. For each of the following rule, the path will
therefore be prefixed by `/domain-name-rule`.

### List domain name rules

Path: `/list`

GET parameters (all optionals):

 * `kind`: The kind of the domain rule key (should be one of
   `exact`, `suffix` or `regex`.
 * `match`: A regular expression applied on the domain rule match
    specification.
 * `replacement`: A regular expression applied on the domain rule
    replacement specification.

Returns the list of domain names rules matching the parameters. If a criteria is
omitted all rules will match this criteria.

Example:

```
GET /domain-name-rule/list?kind=exact HTTP/1.1
Host: management.invalid
```

### Add a domain name rule

Path: `/add`

POST parameters:

 * `kind`: The kind of match string. Must be one of "exact", "suffix"
   or "regex".
 * `match`: The match string used to check whether the rule matches the
   current URL.
 * `replacement` (optional): The replacement string used to modify the URL part
   matching the rule.
 * `http-code` (optional): The HTTP Status code to be sent along for
   this redirection.
 * `position` (optional): Position at which the rule will be inserted
   in the rule list. If none is provided, the rule is inserted at the beginning
   of the list.

Example:

```
POST /domain-name-rule/add HTTP/1.1
Host: management.invalid
Content-type: application/x-www-form-urlencoded

kind=exact&match=www.domain.example&position=3
```

### Remove a domain name rule

Path: `/remove`

GET parameters:

 * `kind`: The kind of the rule.
 * `match`: The match spec of the rule.

POST parameter:

 * `confirmed`: This parameter must be non-nil.

Example:

```
POST /domain-name-rule/remove?kind=exact&match=www.domain.example HTTP/1.1
Host: management.invalid
Content-type: application/x-www-form-urlencoded

confirmed=OK
```

### Update a domain name rule

Path: `/update`

GET parameters:

 * `kind`: The kind of the rule.
 * `match`: The match spec of the rule.

POST parameters (all optional):

  * `new-kind`: The new kind to use for the rule from now on.
  * `new-match`: The new match spec to use for the rule from now on.
  * `new-replacement`: The new replacement to use for the rule from now
    on.
  * `new-http-code`: The new HTTP Status code to be used for this rule
    redirection from now on.

## URI rule management

The URI rule management operations are in the "folder"
`/uri-rule/`. For each of the following rule, the path will
therefore be prefixed by `/uri-rule`.

All this operations need to know on wich parent domain name rule they operate.
Thus, each rule has two common GET parameters:

 * `domain-name-kind`: The kind of the domain name rule.
 * `domain-name-match`: The match spec of the domain name rule.

### List URI rules

Path: `/list`

GET parameters (all optionals):

 * `kind`: The kind of the domain rule key (should be one of
   `exact`, `prefix` or `regex`.
 * `match`: A regular expression applied on the domain rule match
    specification.
 * `replacement`: A regular expression applied on the domain rule
    replacement specification.

Returns the list of URI rules matching the parameters. If a criteria is omitted
all rules will match this criteria.

Example:

```
GET /uri-rule/list?domain-name-kind=exact&domain-name-match=www.domain.example&kind=exact HTTP/1.1
Host: management.invalid
```

### Add a URI rule

Path: `/add`

POST parameters:

 * `kind`: The kind of match string. Must be one of "exact", "suffix"
   or "regex".
 * `match`: The match string used to check whether the rule matches the
   current URL.
 * `replacement` (optional): The replacement string used to modify the URL part
   matching the rule.
 * `http-code` (optional): The HTTP Status code to be sent along for
   this redirection.
 * `position` (optional): Position at which the rule will be inserted
   in the rule list. If none is provided, the rule is inserted at the beginning
   of the list.

Example:

```
POST /uri-rule/add?domain-name-kind=exact&domain-name-match=www.domain.example HTTP/1.1
Host: management.invalid
Content-type: application/x-www-form-urlencoded

kind=prefix&match=%2Ffoo&position=2
```

### Remove a URI rule

Path: `/remove`

GET parameters:

 * `kind`: The kind of the rule.
 * `match`: The match spec of the rule.

POST parameter:

 * `confirmed`: This parameter must be non-nil.

Example:

```
POST /uri-rule/remove?domain-name-kind=exact&domain-name-match=www.domain.example&kind=exact&match=www.domain.example HTTP/1.1
Host: management.invalid
Content-type: application/x-www-form-urlencoded

confirmed=OK
```

### Update a URI rule

Path: `/update`

GET parameters:

 * `kind`: The kind of the rule.
 * `match`: The match spec of the rule.

POST parameters (all optional):

  * `new-kind`: The new kind to use for the rule from now on.
  * `new-match`: The new match spec to use for the rule from now on.
  * `new-replacement`: The new replacement to use for the rule from now
    on.
  * `new-http-code`: The new HTTP Status code to be used for this rule
    redirection from now on.

# Data Storage

The data are stored in memory by a hierarchy of classes, check `redirection-rule`
and subclasses.

# Limitations

 * The implementation restricted part should be extracted away in another
   package/project (TODO).

 * No current provision for redirecting to another port or protocol (TODO).

 * No query string support (TODO).

 * Because of the inherent multiple domain names property of the project,
  Chessire supports only HTTP, no HTTPS now or in the forseenable future is
  planned.

 * No offline rule management.

# License

This software has been originally developed by Mathieu Lemoine
<mlemoine@mentel.com> and is the sole property of Mentel Inc. (www.mentel.com).
This software is distributed under the terms of the 3-clause BSD, stated as
follow:

Copyright © 2012, Mathieu Lemoine <mlemoine@mentel.com>, Mentel Inc.
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.
 * Neither the name of "Mentel Inc." nor the names of its contributors may be
   used to endorse or promote products derived from this software without
   specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
