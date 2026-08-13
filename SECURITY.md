# Security policy

`quasiquotes-scala3` is experimental research software. It parses and lowers
Scala source fragments and includes exact-compiler integration code; it has not
received a production security review.

## Supported versions

No released version is currently supported. Until a first experimental release
exists, security fixes are developed on `main`. After release, the versioning
policy in [Versioning and stability](docs/VERSIONING_AND_STABILITY.md) applies.

## Reporting

No private security-reporting channel is currently offered or promised for
this experimental research stage. Non-sensitive security questions may use
the ordinary issue tracker when it is available. Do not put secrets, exploit
details, sensitive repository data, or private source in a public issue merely
to obtain maintainer attention.

A private reporting channel may be added later. Until then, this policy does
not invent an address, form, confidential intake mechanism, response service,
or availability promise.

## Scope and expectations

Reports should identify the affected module and compiler version, provide a
minimal reproduction when safe, and distinguish parser/frontend behavior from
the unpublished `dottyInternal` integration layer. Response times and security
support levels are not guaranteed for this experimental project. No production
security review or response SLA is promised.
