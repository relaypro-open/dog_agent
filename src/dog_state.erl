-module(dog_state).

-include("dog.hrl").

-export([
         from_map/1, 
         to_group_routing_key/1,
         to_host_routing_key/1, 
         to_map/1
        ]).

-export([
         get_os_distribution/1,
         get_os_version/1,
         set_ec2_region/2,
         set_os_distribution/2,
         set_os_version/2,
         dog_state/25,
         get_docker_container_ids/1,
         get_ec2_availability_zone/1,
         get_ec2_instance_id/1,
         get_ec2_instance_tags/1,
         get_ec2_owner_id/1,
         get_ec2_region/1,
         get_ec2_security_group_ids/1,
         get_ec2_subnet_id/1,
         get_ec2_vpc_id/1,
         get_environment/1,
         get_group/1, 
         get_hash4_ipsets/1, 
         get_hash4_iptables/1,
         get_hash6_ipsets/1, 
         get_hash6_iptables/1, 
         get_hostkey/1,
         get_hostname/1, 
         get_interfaces/1, 
         get_ipset_hash/1,
         get_location/1, 
         get_provider/1, 
         get_updatetype/1,
         get_version/1, 
         set_docker_container_ids/2,
         set_ec2_availability_zone/2,
         set_ec2_instance_id/2,
         set_ec2_instance_tags/2,
         set_ec2_owner_id/2,
         set_ec2_security_group_ids/2,
         set_ec2_subnet_id/2,
         set_ec2_vpc_id/2,
         set_environment/2, 
         set_group/2,
         set_hash4_ipsets/2, 
         set_hash4_iptables/2,
         set_hash6_ipsets/2, 
         set_hash6_iptables/2, 
         set_hostkey/2,
         set_hostname/2,
         set_interfaces/2, 
         set_ipset_hash/2,
         set_location/2, 
         set_provider/2, 
         set_updatetype/2,
         set_version/2
        ]).

-type docker_container_ids() :: [binary()].
-type ec2_availability_zone() :: string().
-type ec2_instance_id() :: string().
-type ec2_instance_tags() :: map().
-type ec2_owner_id() :: string().
-type ec2_region() :: string().
-type ec2_security_group_ids() :: list().
-type ec2_subnet_id() :: string().
-type ec2_vpc_id() :: string().
-type environment() :: binary().
-type group() :: binary().
-type hash() :: binary().
-type hostkey() :: binary().
-type hostname() :: binary().
-type interfaces() :: [tuple()].
-type location() :: binary().
-type os_distribution() :: binary().
-type os_version() :: binary().
-type provider() :: binary().
-type updatetype() :: atom().
-type version() :: binary().

-record(dog_state,
    {group, name, location, environment, hostkey,
     interfaces, version, hash4_ipsets, hash6_ipsets,
     hash4_iptables, hash6_iptables, provider, updatetype,
     ipset_hash,ec2_region,ec2_instance_id,ec2_availability_zone,ec2_security_group_ids,ec2_owner_id,
     ec2_instance_tags,os_distribution,os_version,ec2_vpc_id,ec2_subnet_id,docker_container_ids}).

-type dog_state() :: #dog_state{}.

dog_state(Group, Hostname, Location, Environment,
      Hostkey, Interfaces, Version, Hash4Ipsets, Hash6Ipsets,
      Hash4Iptables, Hash6Iptables, Provider, UpdateType,
      IpsetHash, Ec2Region, Ec2InstanceId, Ec2AvailabilityZone, 
      Ec2SecurityGroupIds, Ec2OwnerId, Ec2InstanceTags,
      OS_Distribution, OS_Version, Ec2VpcId, Ec2SubnetId,DockerContainerIds) ->
    #dog_state{group = Group, name = Hostname,
           location = Location, environment = Environment,
           hostkey = Hostkey, interfaces = Interfaces,
           version = Version, hash4_ipsets = Hash4Ipsets,
           hash6_ipsets = Hash6Ipsets,
           hash4_iptables = Hash4Iptables,
           hash6_iptables = Hash6Iptables, provider = Provider,
           updatetype = UpdateType, ipset_hash = IpsetHash,
	   ec2_region = Ec2Region,
           ec2_instance_id = Ec2InstanceId,
           ec2_availability_zone = Ec2AvailabilityZone,
           ec2_security_group_ids = Ec2SecurityGroupIds,
           ec2_owner_id = Ec2OwnerId,
           ec2_instance_tags = Ec2InstanceTags,
	   os_distribution = OS_Distribution,
	   os_version = OS_Version,
	   ec2_vpc_id = Ec2VpcId,
	   ec2_subnet_id = Ec2SubnetId,
       docker_container_ids = DockerContainerIds
              }.

-spec get_group(State :: dog_state()) -> binary().
get_group(State) -> State#dog_state.group.

-spec set_group(State :: dog_state(),
        Group :: group()) -> dog_state().
set_group(State, Group) ->
    State#dog_state{group = Group}.

-spec get_hostname(State :: dog_state()) -> binary().
get_hostname(State) -> State#dog_state.name.

-spec set_hostname(State :: dog_state(),
           Hostname :: hostname()) -> dog_state().
set_hostname(State, Hostname) ->
    State#dog_state{name = Hostname}.

-spec get_location(State :: dog_state()) -> binary().
get_location(State) -> State#dog_state.location.

-spec set_location(State :: dog_state(),
           Location :: location()) -> dog_state().
set_location(State, Location) ->
    State#dog_state{location = Location}.

-spec get_os_distribution(State :: dog_state()) -> binary().
get_os_distribution(State) -> State#dog_state.location.

-spec set_os_distribution(State :: dog_state(),
           Location :: os_distribution()) -> dog_state().
set_os_distribution(State, Location) ->
    State#dog_state{os_distribution = Location}.

-spec get_os_version(State :: dog_state()) -> binary().
get_os_version(State) -> State#dog_state.location.

-spec set_os_version(State :: dog_state(),
           Location :: os_version()) -> dog_state().
set_os_version(State, Location) ->
    State#dog_state{os_version = Location}.

-spec get_environment(State :: dog_state()) -> binary().
get_environment(State) -> State#dog_state.environment.

-spec set_environment(State :: dog_state(),
              Environment :: environment()) -> dog_state().
set_environment(State, Environment) ->
    State#dog_state{environment = Environment}.

-spec get_hostkey(State :: dog_state()) -> binary().
get_hostkey(State) -> State#dog_state.hostkey.

-spec set_hostkey(State :: dog_state(),
          Hostkey :: hostkey()) -> dog_state().
set_hostkey(State, Hostkey) ->
    State#dog_state{hostkey = Hostkey}.

-spec get_interfaces(State :: dog_state()) -> [tuple()].
get_interfaces(State) -> State#dog_state.interfaces.

-spec set_interfaces(State :: dog_state(),
             Interfaces :: interfaces()) -> dog_state().
set_interfaces(State, Interfaces) ->
    State#dog_state{interfaces = Interfaces}.

-spec get_version(State :: dog_state()) -> binary().
get_version(State) -> State#dog_state.version.

-spec set_version(State :: dog_state(),
          Version :: version()) -> dog_state().
set_version(State, Version) ->
    State#dog_state{version = Version}.

-spec get_hash4_ipsets(State ::
               dog_state()) -> binary().
get_hash4_ipsets(State) -> State#dog_state.hash4_ipsets.

-spec set_hash4_ipsets(State :: dog_state(),
               Hash4 :: hash()) -> dog_state().
set_hash4_ipsets(State, Hash4) ->
    State#dog_state{hash4_ipsets = Hash4}.

-spec get_hash6_ipsets(State ::
               dog_state()) -> binary().
get_hash6_ipsets(State) -> State#dog_state.hash6_ipsets.

-spec set_hash6_ipsets(State :: dog_state(),
               Hash6 :: hash()) -> dog_state().
set_hash6_ipsets(State, Hash6) ->
    State#dog_state{hash6_ipsets = Hash6}.

-spec get_hash4_iptables(State ::
                 dog_state()) -> binary().
get_hash4_iptables(State) ->
    State#dog_state.hash4_iptables.

-spec set_hash4_iptables(State :: dog_state(),
             Hash4 :: hash()) -> dog_state().
set_hash4_iptables(State, Hash4) ->
    State#dog_state{hash4_iptables = Hash4}.

-spec get_hash6_iptables(State ::
                 dog_state()) -> binary().
get_hash6_iptables(State) ->
    State#dog_state.hash6_iptables.

-spec set_hash6_iptables(State :: dog_state(),
             Hash6 :: hash()) -> dog_state().
set_hash6_iptables(State, Hash6) ->
    State#dog_state{hash6_iptables = Hash6}.

-spec get_provider(State :: dog_state()) -> binary().
get_provider(State) -> State#dog_state.provider.

-spec set_provider(State :: dog_state(),
           Provider :: provider()) -> dog_state().
set_provider(State, Provider) ->
    State#dog_state{provider = Provider}.

-spec get_updatetype(State :: dog_state()) -> atom().
get_updatetype(State) -> State#dog_state.updatetype.

-spec set_updatetype(State :: dog_state(),
             Update :: updatetype()) -> dog_state().
set_updatetype(State, UpdateType) ->
    State#dog_state{updatetype = UpdateType}.

-spec get_ipset_hash(State :: dog_state()) -> atom().
get_ipset_hash(State) -> State#dog_state.ipset_hash.

-spec set_ipset_hash(State :: dog_state(),
             IpsetHash :: hash()) -> dog_state().
set_ipset_hash(State, IpsetHash) ->
    State#dog_state{ipset_hash = IpsetHash}.

-spec get_ec2_region(State :: dog_state()) -> string().
get_ec2_region(State) -> State#dog_state.ec2_region.

-spec set_ec2_region(State :: dog_state(),
            Ec2InstanceId :: ec2_region()) -> dog_state().
set_ec2_region(State, Ec2InstanceId) ->
    State#dog_state{ec2_region = Ec2InstanceId}.

-spec get_ec2_instance_id(State :: dog_state()) -> string().
get_ec2_instance_id(State) -> State#dog_state.ec2_instance_id.

-spec set_ec2_instance_id(State :: dog_state(),
            Ec2InstanceId :: ec2_instance_id()) -> dog_state().
set_ec2_instance_id(State, Ec2InstanceId) ->
    State#dog_state{ec2_instance_id = Ec2InstanceId}.

-spec get_ec2_availability_zone(State :: dog_state()) -> string().
get_ec2_availability_zone(State) -> State#dog_state.ec2_availability_zone.

-spec set_ec2_availability_zone(State :: dog_state(),
            Ec2AvailabilityZone :: ec2_availability_zone()) -> dog_state().
set_ec2_availability_zone(State, Ec2AvailabilityZone) ->
    State#dog_state{ec2_availability_zone = Ec2AvailabilityZone}.

-spec get_ec2_security_group_ids(State :: dog_state()) -> list().
get_ec2_security_group_ids(State) -> State#dog_state.ec2_security_group_ids.

-spec set_ec2_security_group_ids(State :: dog_state(),
            Ec2SecurityGroupIds :: ec2_security_group_ids()) -> dog_state().
set_ec2_security_group_ids(State, Ec2SecurityGroupIds) ->
    State#dog_state{ec2_security_group_ids = Ec2SecurityGroupIds}.

-spec get_ec2_subnet_id(State :: dog_state()) -> string().
get_ec2_subnet_id(State) -> State#dog_state.ec2_subnet_id.

-spec set_ec2_subnet_id(State :: dog_state(),
            Ec2SubnetId :: ec2_subnet_id()) -> dog_state().
set_ec2_subnet_id(State, Ec2SubnetId) ->
    State#dog_state{ec2_subnet_id = Ec2SubnetId}.

-spec get_ec2_vpc_id(State :: dog_state()) -> string().
get_ec2_vpc_id(State) -> State#dog_state.ec2_vpc_id.

-spec set_ec2_vpc_id(State :: dog_state(),
            Ec2VpcId :: ec2_vpc_id()) -> dog_state().
set_ec2_vpc_id(State, Ec2VpcId) ->
    State#dog_state{ec2_vpc_id = Ec2VpcId}.

-spec get_ec2_owner_id(State :: dog_state()) -> list().
get_ec2_owner_id(State) -> State#dog_state.ec2_owner_id.

-spec set_ec2_owner_id(State :: dog_state(),
            Ec2OwnerId :: ec2_owner_id()) -> dog_state().
set_ec2_owner_id(State, Ec2OwnerId) ->
    State#dog_state{ec2_owner_id = Ec2OwnerId}.

-spec get_ec2_instance_tags(State :: dog_state()) -> map().
get_ec2_instance_tags(State) -> State#dog_state.ec2_instance_tags.

-spec set_ec2_instance_tags(State :: dog_state(),
            Ec2InstanceTags :: ec2_instance_tags()) -> dog_state().
set_ec2_instance_tags(State, Ec2InstanceTags) ->
    State#dog_state{ec2_instance_tags = Ec2InstanceTags}.

-spec get_docker_container_ids(State :: dog_state()) -> map().
get_docker_container_ids(State) -> State#dog_state.docker_container_ids.

-spec set_docker_container_ids(State :: dog_state(),
            DockerContainerIds :: docker_container_ids()) -> dog_state().
set_docker_container_ids(State, DockerContainerIds) ->
    State#dog_state{docker_container_ids = DockerContainerIds}.

to_map(State) ->
    Interfaces = jsx:encode(State#dog_state.interfaces),
    #{<<"name">> => State#dog_state.name,
      <<"interfaces">> => Interfaces,
      <<"group">> => State#dog_state.group,
      <<"location">> => State#dog_state.location,
      <<"environment">> => State#dog_state.environment,
      <<"hostkey">> => State#dog_state.hostkey,
      <<"version">> => State#dog_state.version,
      <<"hash4_ipsets">> => State#dog_state.hash4_ipsets,
      <<"hash6_ipsets">> => State#dog_state.hash6_ipsets,
      <<"hash4_iptables">> => State#dog_state.hash4_iptables,
      <<"hash6_iptables">> => State#dog_state.hash6_iptables,
      <<"provider">> => State#dog_state.provider,
      <<"updatetype">> => State#dog_state.updatetype,
      <<"ipset_hash">> => State#dog_state.ipset_hash,
      <<"ec2_region">> => State#dog_state.ec2_region,
      <<"ec2_instance_id">> => State#dog_state.ec2_instance_id,
      <<"ec2_availability_zone">> => State#dog_state.ec2_availability_zone,
      <<"ec2_security_group_ids">> => State#dog_state.ec2_security_group_ids,
      <<"ec2_owner_id">> => State#dog_state.ec2_owner_id,
      <<"ec2_instance_tags">> => State#dog_state.ec2_instance_tags,
      <<"os_distribution">> => State#dog_state.os_distribution,
      <<"os_version">> => State#dog_state.os_version,
      <<"ec2_vpc_id">> => State#dog_state.ec2_vpc_id,
      <<"ec2_subnet_id">> => State#dog_state.ec2_subnet_id,
      <<"docker_container_ids">> => State#dog_state.docker_container_ids
     }.

from_map(StateMap) ->
    ?LOG_DEBUG("StateMap: ~p",[StateMap]),
    #dog_state{
        name = maps:get(<<"name">>,StateMap), 
        interfaces = maps:get(<<"interfaces">>,StateMap), 
        group = maps:get(<<"group">>,StateMap), 
        location = maps:get(<<"location">>,StateMap), 
        environment = maps:get(<<"environment">>,StateMap), 
        hostkey = maps:get(<<"hostkey">>,StateMap),
        version = maps:get(<<"version">>,StateMap),
        hash4_ipsets = maps:get(<<"hash4_ipsets">>,StateMap),
        hash6_ipsets = maps:get(<<"hash6_ipsets">>,StateMap),
        hash4_iptables = maps:get(<<"hash4_iptables">>,StateMap),
        hash6_iptables = maps:get(<<"hash6_iptables">>,StateMap),
        provider = maps:get(<<"provider">>,StateMap),
        updatetype = maps:get(<<"updatetype">>,StateMap),
        ipset_hash = maps:get(<<"ipset_hash">>,StateMap),
        ec2_region = maps:get(<<"ec2_region">>,StateMap),
        ec2_instance_id = maps:get(<<"ec2_instance_id">>,StateMap),
        ec2_availability_zone = maps:get(<<"ec2_availability_zone">>,StateMap),
        ec2_security_group_ids = maps:get(<<"ec2_security_group_ids">>,StateMap),
        ec2_owner_id = maps:get(<<"ec2_owner_id">>,StateMap),
        ec2_instance_tags = maps:get(<<"ec2_instance_tags">>,StateMap),
        os_distribution = maps:get(<<"os_distribution">>,StateMap),
        os_version = maps:get(<<"os_version">>,StateMap),
        ec2_vpc_id = maps:get(<<"ec2_vpc_id">>,StateMap),
        ec2_subnet_id = maps:get(<<"ec2_subnet_id">>,StateMap),
        docker_container_ids = maps:get(<<"docker_container_ids">>,StateMap)
    }.

to_group_routing_key(State) ->
    binary:list_to_bin([State#dog_state.environment,
            <<".">>, State#dog_state.location, <<".">>,
            State#dog_state.group, <<".">>, <<"*">>]).

to_host_routing_key(State) ->
    binary:list_to_bin([State#dog_state.environment,
            <<".">>, State#dog_state.location, <<".">>, <<"*">>,
            <<".">>, State#dog_state.hostkey]).
