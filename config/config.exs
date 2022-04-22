import Config

# This configuration is loaded before any dependency and is restricted
# to this project. If another project depends on this project: this
# file won't be loaded nor affect the parent project. For this reason,
# if you want to provide default values for your application for
# third-party users: it should be done in your "mix.exs" file.

# You can configure your application as:
#
#     config :dog: key: :value
#
# and access this configuration in your application as:
#
#     Application.get_env(:dog: :key)
#
# You can also configure a third-party app:
#
#     config :logger: level: :info
#

# It is also possible to import configuration files: relative to this
# directory. For example: you can emulate configuration per environment
# by uncommenting the line below and defining dev.exs: test.exs and such.
# Configuration from the imported file will override the ones defined
# here (which is why it is important to import them last).
#
#     import_config "#Mix.env().exs"
#
config :kernel, inet_dist_use_interface: {127,0,0,1}

config :dog, 
        version: "7342b0908e4dbf8daf70ccf3f18422c7d654c504",
        enforcing: true,
        use_ipsets: true

#config :sync, 
#        growl: :none,
#        log: [:all],
#        non_descendants: :fix,
#        executable: :auto,
#        whitelisted_modules: [],
#        excluded_modules: []
# Stop lager redirecting :error_logger messages
config :lager, :error_logger_redirect, false

# Stop lager removing Logger's :error_logger handler
config :lager, :error_logger_whitelist, [Logger.ErrorHandler]

# Stop lager writing a crash log
config :lager, :crash_log, false

config :lager,
  log_root: '/var/log/dog',
  handlers: [
    lager_console_backend: :info,
    lager_file_backend: [file: "error.log", level: :error],
    lager_file_backend: [file: "console.log", level: :info]
  ]

#config :lager, 
#        handlers: [
#            {:lager_file_backend, [{:file, '/var/log/dog/debug.log'}, {:level, :debug}]},
#            {:lager_file_backend, [{:file, '/var/log/dog/error.log'}, {:level, :error}]},
#            {:lager_file_backend, [{:file, '/var/log/dog/console.log'}, {:level, :info }]}
#        ],
#        crash_log: '/var/log/dog/crash.log',
#        tracefiles: [],
#        async_threshold: 10000,
#        sieve_threshold: 5000,
#        sieve_window: 100,
#        colored: true

#config :thumper, 
#        substitution_rules: [
#           {:fqdn, {:dog_interfaces,:fqdn,[]}},
#           {:environment, {:dog_config,:environment,[]}},
#           {:location, {:dog_config,:location,[]}},
#           {:group, {:dog_config,:group,[]}},
#           {:hostkey, {:dog_config,:hostkey,[]}}
#        ],
#        thumper_svrs: [:default, :publish],
#        brokers: [
#            {:default, [
#                {:rabbitmq_config,
#                   [
#                        {:host, "dog-ubuntu-server.lxd"},
#                        {:port, 5673},
#                        {:api_port, 15672},
#                        {:virtual_host, <<"dog">>},
#                        {:user, <<"dog">>},
#                        {:password, <<"327faf06-c3f5-11e7-9765-7831c1be5b34">>},
#                     {:ssl_options, [
#                                       {:cacertfile, "/var/consul/data/pki/certs/ca.crt"},
#                                       {:certfile, "/var/consul/data/pki/certs/server.crt"},
#                                       {:keyfile, "/var/consul/data/pki/private/server.key"},
#                                       {:verify, :verify_peer},
#                                       {:server_name_indication, :disable},
#                                       {:fail_if_no_peer_cert, true}
#                                      ]},
#                     {:broker_config,
#                        {:thumper_tx, {:callback, {:dog_config, :broker_config, []}}}
#                     }
#                   ]}]},
#            {:publish, [{:rabbitmq_config, :default}]}
#            ],
#        queuejournal:
#            [
#                {:enabled, false},
#                {:dir, "/opt/dog/queuejournal"},
#                {:memqueue_max, 10000},
#                {:check_journal, true}
#            ]

config :turtle,
        connection_config: [
            %{
                :conn_name => :default,
                :username => 'dog',
                :password => '327faf06-c3f5-11e7-9765-7831c1be5b34',
                :virtual_host => 'dog',
                :ssl_options => [
                               {:cacertfile, '/var/consul/data/pki/certs/ca.crt'},
                               {:certfile, '/var/consul/data/pki/certs/server.crt'},
                               {:keyfile, '/var/consul/data/pki/private/server.key'},
                               {:verify, :verify_peer},
                               {:server_name_indication, :disable},
                               {:fail_if_no_peer_cert, true}
                              ],
                :deadline => 300000,
                :connections => [
                    {:main, [
                      {'dog-ubuntu-server.lxd', 5673 } 
                    ]}
                ]
            }
        ]	

config :erldocker,
docker_http: <<"http+unix://%2Fvar%2Frun%2Fdocker.sock">>

#config :thumper,
#  substitution_rules: [
#    cluster: {:edb, :get_cluster_id, []}
#  ],
#  thumper_svrs: [:default, :publish],
#  brokers: [
#    default: [
#      rabbitmq_config: [
#        host: 'dog-ubuntu-server.lxd',
#        port: 5673,
#        api_port: 15672,
#        virtual_host: "dog",
#        user: "dog_trainer",
#        password: "327faf06-c3f5-11e7-9765-7831c1be5b34",
#        ssl_options: [
#          cacertfile: '/var/consul/data/pki/certs/ca.crt',
#          certfile: '/var/consul/data/pki/certs/server.crt',
#          keyfile: '/var/consul/data/pki/private/server.key',
#          verify: :verify_peer,
#          server_name_indication: :disable,
#          fail_if_no_peer_cert: true
#        ],
#        broker_config: {:thumper_tx, "/opt/dog_trainer_ex/priv/broker.tx" }
#      ]
#    ],
#    publish: [rabbitmq_config: :default]
#  ],
#  queuejournal: [
#    enabled: true,
#    dir: '/opt/dog_trainer_ex/queuejournal',
#    memqueue_max: 10000,
#    check_journal: true
#  ]
#
