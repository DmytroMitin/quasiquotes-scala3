# Security policy

`quasiquotes-scala3` is experimental research software. It parses and lowers
Scala source fragments and includes exact-compiler integration code; it has not
received a production security review.

## Supported versions

No released version is currently supported. Until a first experimental release
exists, security fixes are developed on `main`. After release, the versioning
policy in [Versioning and stability](docs/VERSIONING_AND_STABILITY.md) applies.

## Reporting

A private security-reporting channel has not yet been established. Do not put
secrets, exploit details, or sensitive repository data in a public issue.
Non-sensitive security questions may use the ordinary issue tracker after the
repository becomes public.

Establishing and publishing a private reporting channel is a required gate
before public visibility. This file deliberately does not invent an email
address or promise an unavailable service.

## Scope and expectations

Reports should identify the affected module and compiler version, provide a
minimal reproduction when safe, and distinguish parser/frontend behavior from
the unpublished `dottyInternal` integration layer. Response times and security
support levels are not guaranteed for this experimental project.
