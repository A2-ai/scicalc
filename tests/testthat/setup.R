# Keep audit logs out of the package tree during tests.
options(scicalc.audit_log = file.path(tempdir(), "scicalc-test-audit.log"))
