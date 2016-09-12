handle SIGUSR1 nostop
python
import sys, os

# 3 cases: 1. Native, 2. VM, 3. Cygwin
if os.path.isdir(os.path.expanduser('~/files')):
	sys.path.insert(
		0, os.path.expanduser('~/files/projects/third_party/gdb-printers'))

# import the printers, use different files depending on python version
try:
	if sys.version_info.major == 2:
		from libstdcxx.v6.printers import register_libstdcxx_printers
	elif sys.version_info.major == 3:
		from libstdcxx.v6.printers3 import register_libstdcxx_printers
	register_libstdcxx_printers (None)
except ImportError:
	pass
end
