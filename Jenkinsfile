#!/usr/bin/env groovy
pipeline {
    agent none

        environment {
            tag = VersionNumber(versionNumberString: '${BUILD_TIMESTAMP}');
            // job environment variables
            BUILD_ENV = "${build_stream}"
                // this is a result of a backwards-incompatible change from JENKINS-24380
            BUILD_ID = "${VersionNumber(projectStartDate: '1970-01-01', versionNumberString: '${BUILD_DATE_FORMATTED, \"yyyy-MM-dd_H-m-s\"}')}"
        }

    parameters {
        booleanParam(name: 'deploy',
                     defaultValue: false,
                     description: 'Whether or not to deploy this new build to the environment selected below.')
        choice(name: 'branch',
               choices: ['main','feature/shutdown_notify'],
               description: 'The source branch to compile.')
        choice(name: 'env',
               choices: ['mob_qa','mob_pro','beta_qa','stage.qa','api.qa','api.pro'],
               description: 'If deploying, which ansible environment to deploy to.  Determines deployment target')
        choice(name: 'dog_env',
               choices: ['qa','pro'],
               description: 'If deploying, which dog environment to deploy to.  Determines which dog_trainer this build will connect to')
        string(name: 'target',
               defaultValue: '',
               description: 'The target host/group.')
        string(name: 'flags',
               defaultValue: '--tags upgrade',
               description: 'ansible flags')
        choice(name: 'erlang_version',
               choices: ['24.3.4.2','25.3.2.12','23.3.4.3'],
               description: 'The erlang_version dog_agent_ex will be built with')
        choice(name: 'release_path',
               choices: ['deployable/rel/dog_agent/','test/rel/dog_agent/'],
               description: 'deployable or test type of artifact')
    }

    stages {

        stage('Matrix Build') {
            matrix {
                agent {
                    docker { image "${DOCKER_IMAGE}" }
                }
                axes {
                    axis {
                        name 'DOCKER_IMAGE'
                            values 'relaypro/erlang-focal:master-latest',
                            'relaypro/erlang-xenial:master-latest',
                            'relaypro/erlang-noble:master-latest'
                    }
                }

                stages {
                    stage ('Checkout') {
                        steps {
                            checkout([$class: 'GitSCM', branches: [[name: '${branch}']], doGenerateSubmoduleConfigurations: false, extensions: [], submoduleCfg: [], userRemoteConfigs: [[credentialsId: 'admin', url: 'https://github.com/relaypro-open/dog_agent.git']]]) 
                        }
                    }
                    stage('Build') {
                        steps{

                            sh """#!/bin/bash -x
                                echo "env: \$env"
                                echo "dog_env: \$dog_env"

                                if [[ \$env == *qa ]]; then
                                    build_env="qa"
                                elif [[ \$env == *pro ]]; then
                                    build_env="pro"
                                elif [[ \$env == *dev ]]; then
                                    build_env="dev"
                                fi
                                
                                if [[ \$DOCKER_IMAGE == 'relaypro/erlang-xenial:master-latest' ]]; then
                                    build_suffix='ubuntu'
                                elif [[ \$DOCKER_IMAGE == 'relaypro/erlang-focal:master-latest' ]]; then    
                                    build_suffix='ubuntu-20-04'
                                elif [[ \$DOCKER_IMAGE == 'relaypro/erlang-noble:master-latest' ]]; then    
                                    build_suffix='ubuntu-24-04'
                                fi
                                
                                ls /opt/kerl/lib
                                . /opt/kerl/lib/\$erlang_version/activate
                                which erl
                                
                                echo "build_env: \$build_env"
                                
                                rm -rf _build
                                
                                export erts_version=\$(scripts/erts_version.escript)
                                config/make_dog_start.sh
                                sed -i "s/.* %% relflow-release-version-marker/\\"$dog_env-$BUILD_ID\\" %% relflow-release-version-marker/g" rebar.config
                                sed -i "s/NOT_SET/$dog_env-$BUILD_ID/"g config/sys.config.etc
#rm -f rebar3
#wget https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3
                                ./rebar3 as \$dog_env tar
                                mv _build/*/rel/dog/dog-\$dog_env-\$BUILD_ID.tar.gz \$dog_env-\$BUILD_ID.\$build_suffix.tar.gz
                                """

                        }

                        post {
                            success {
                                archiveArtifacts allowEmptyArchive: false, artifacts: '*.tar.gz', caseSensitive: true, defaultExcludes: true, fingerprint: false
                            }


                        }
                    }

                    stage('Upload artifact to S3') {
                        steps {
                            withAWS(credentials: 'aws-iam-user/product-jenkins-artifact-uploads') {
                                s3Upload bucket:'product-builds', path: 'dog/' + params.release_path, includePathPattern: '*.tar.gz'
                            }

                        }
                    } 

                    stage('Cleanup') {
                        steps {
                            cleanWs(cleanWhenNotBuilt: false,
                                    deleteDirs: true,
                                    disableDeferredWipeout: true,
                                    notFailBuild: true)
                        }
                    }


                    stage('Deploy') {
                        when {
                            expression { params.deploy == true }
                        }
                        steps {
                            build job: '/playbyplay/pbp-common-deploy', parameters: [
                                string(name: 'deployEnv', value: "${params.env}"),
                                string(name: 'app', value: 'dog'),
                                string(name: 'target', value: "${params.target}"),
                                //string(name: 'ansibleFlags', value: "--tags ci -ebuild_stream=${params.buildStream}"),
                                string(name: 'ansibleFlags', value: "-e release_path=${params.release_path} -eerlang_version=$erlang_version ${params.flags}"),
                                string(name: 'release_path', value: "${params.release_path}"),
                                string(name: 'erts_version', value: "${params.erts}")
                            ]
                        }
                    }

                }
            }
        }
    }
    post {
        changed {
            emailext(
                    to: 'dgulino@relaypro.com',
                    body: '${DEFAULT_CONTENT}', 
                    mimeType: 'text/html',
                    subject: '${DEFAULT_SUBJECT}',
                    replyTo: '$DEFAULT_REPLYTO'    
                    )
        }

    }
}
