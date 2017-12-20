Notes

- The tests can be performed by running:

  performanceTest.py

- The hardcoded timeout value can be found in

  TestConfig.py

- Test program needs the Python pexpect module
  - Freely available (use google)
  - Debian/Ubuntu: apt-get install python-pexpect

- Test program needs 'cpp' (C preprocessor) for avispa_tls_spdl.py

- The user of this test program should make sure the following programs
  are in path:

  avispa		(Avispa tools front)
  hlpsl2if		(Avispa - needed to exclude the translation phase from the time measurements)
  scyther-linux		(Scyther)
  analyzer		(ProVerif)

- Protocols

  For sure in the list:

  nspk		Needham-Schroeder public key
  fix_nspk	Lowe's fix
  avispa_tls	The TLS protocol, as in the Avispa lib
  eke		EKE

