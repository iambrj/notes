- OAuth is about authorization, NOT authentication
- OAuth concepts:
  * Resource owner
  * Resource server
  * Grant type: process used to request and grant authorization
  * Scope: permission that is being requested (read-write access, profile access, etc)
  * Auth server: where grant type and scope are sent for authorization
  * Token: string on successful authorization
  * Claims: details on authorization granted
- OAuth core
  * authorize endpoint: used by end user (resource owner) to grant permission
    for application to access resource. Returns authorization code or access code
  * token endpoint: used by application to trade authorization code for an
    access token
- Optional endpoints:
  * userinfo (OpenID core connect)
  * discovery: gives all URLs and capabilities of OAuth server
  * introspect: get token status
  * revoke: used to deactivate a token
- Grant type decision tree:
  1. For a user? No => Client Credential Grant Type
  2. Browser available? No => Device Grant Type
  3. Server-side only? No => Implicit grant type/Authorization code flow with PKCE
                      Yes => Authorization code flow
- OAuth scope: CRUD or more complicted; scopes are the permissions that we
  request not the endpoints that we use.
- E.g. GitHub scopes: repo, public_repo, repo_deployment, repo:invite, etc
- Some other examples: Google scope, okta
- Some types of tokens :
  * Access (Core RFC 6749)
  * Refresh (Core RFC 6749)
  * ID (OpenID connect). JSON Web Token (JWT).
- JWTs need validating.
- Every JWT is made of 3 parts:
  * Header (algorithm used to sign)
  * Payload (key-value pairs called claims)
  * Signature (authentication)
- Do not store sensitive information in JWTs
- Authorization Code Flow:
  * 
