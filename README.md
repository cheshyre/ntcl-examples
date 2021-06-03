NTCL examples repository

Create NTCL_ROOT environment variable: `export NTCL_ROOT=path-to-ntcl-dev-root-directory`

Prerequisites (run make in each directory):
1. ${NTCL_ROOT}/ntcl-util
2. ${NTCL_ROOT}/ntcl-data
3. ${NTCL_ROOT}/ntcl-tensor
4. ${NTCL_ROOT}/ntcl-algorithms

Each directory has it's own set of tests that are compiled using:
`make test`
and run using:
`bin/unittest.x`

To add application:
1. Add application name to the applications variable in `build.info`.
2. Run the build generator:
    `PYTHONPATH=${NTCL_ROOT}/ntcl-build ${NTCL_ROOT}/ntcl-build/bin/create_build_system.py build.info`
3. Create the fortran program in `src/modules/<application name>`
4. Compile applications `make apps`
5. Executable in `bin/<application name>.x`
