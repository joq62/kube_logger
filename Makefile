all:
#	service
	rm -rf ebin/*;
	rm -rf logs;
	erlc -o ebin src/*.erl;
	rm -rf src/*.beam *.beam  test_src/*.beam test_ebin;
	rm -rf  *~ */*~  erl_cra*;
	rm -rf *_specs *_config *.log catalog;
	rm -rf *_pod*;
	rm -rf Mn* kube_logger;
	erlc -o ebin src/*.erl;
	echo done
monitor:
	erlc -o ebin src/monitor_server.erl;
	erlc -o ebin ../interfaces/monitor.erl;
	erl -pa ebin -s monitor start -sname monitor -setcookie abc 
unit_test:
	rm -rf ebin/* src/*.beam *.beam test_src/*.beam test_ebin;
	rm -rf  *~ */*~  erl_cra*;
	rm -rf *_specs *_config *.log *_pod*;
#	support
	cp ../support/src/support.app ebin;
	erlc -o ebin ../support/src/*.erl;
#	kubelet
	erlc -o ebin src/*.erl;
#	test application
	mkdir test_ebin;
	cp test_src/*.app test_ebin;
	erlc -o ebin ../interfaces/*.erl;
	erlc -o test_ebin test_src/*.erl;
	erl -pa ebin -pa test_ebin\
	    -setcookie abc\
	    -sname test_mylog\
	    -run unit_test start_test test_src/test.config
