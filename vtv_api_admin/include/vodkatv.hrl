%%% This is an automatically generated Erlang file.

-record(tagNode,
        {anyAttrs :: any(),
         tagId::integer(),
         name::none|string(),
         parentId::none|integer(),
         position::none|integer()
        }).

-record(createRoomParams,
        {anyAttrs :: any(),
         roomId::string(),
         description::none|string(),
         tag::none|#tagNode{}
        }).

-record(deleteRoomParams,
        {anyAttrs :: any(),
         roomId::none|nonempty_list(string())
        }).

-record(roomType,
        {anyAttrs :: any(),
         roomId::string(),
         description::none|string(),
         tag::none|#tagNode{}
        }).

-record(errorParam,
        {anyAttrs :: any(),
         name::string(),
         value::string()
        }).

-record(errorParams,
        {anyAttrs :: any(),
         param::nonempty_list(#errorParam{})
        }).

-record(error,
        {anyAttrs :: any(),
         code::string(),
         params::none|#errorParams{},
         description::string()
        }).

-record(errors,
        {anyAttrs :: any(),
         error::nonempty_list(#error{})
        }).

-record(rooms,
        {anyAttrs :: any(),
         room::none|nonempty_list(#roomType{}),
         errors::none|#errors{}
        }).

-record(room,
        {anyAttrs :: any(),
         roomId::none|string(),
         description::none|string(),
         tag::none|#tagNode{},
         errors::none|#errors{}
        }).

-record(createDeviceParams,
        {anyAttrs :: any(),
         physicalId::string(),
         deviceClass::none|string(),
         roomId::string(),
         description::none|string(),
         tag::none|#tagNode{}
        }).

-record(findDeviceByIdParams,
        {anyAttrs :: any(),
         deviceId::integer()
        }).

-record(findDevicesParams,
        {anyAttrs :: any(),
         startIndex::integer(),
         count::integer(),
         sortBy::none|string(),
         order::none|string(),
         'query'::none|string()
        }).

-record(findDevicesByRoomParams,
        {anyAttrs :: any(),
         roomId::string()
        }).

-record(findDevicesByTagIdParams,
        {anyAttrs :: any(),
         tagId::integer(),
         startIndex::integer(),
         count::integer(),
         sortBy::none|string(),
         order::none|string(),
         'query'::none|string()
        }).

-record(updateDeviceParams,
        {anyAttrs :: any(),
         id::integer(),
         physicalId::string(),
         deviceClass::none|string(),
         roomId::string(),
         description::none|string(),
         tag::none|#tagNode{}
        }).

-record(deleteDeviceParams,
        {anyAttrs :: any(),
         deviceId::none|nonempty_list(integer())
        }).

-record(deviceType,
        {anyAttrs :: any(),
         id::none|integer(),
         physicalId::none|string(),
         deviceClass::none|string(),
         roomId::none|string(),
         description::none|string(),
         tag::none|#tagNode{},
         version::none|integer(),
         lastUpdateTime::none|string()
        }).

-record(devices,
        {anyAttrs :: any(),
         existsMore::boolean(),
         countTotal::integer(),
         device::none|nonempty_list(#deviceType{}),
         errors::none|#errors{}
        }).

-record(device,
        {anyAttrs :: any(),
         id::none|integer(),
         physicalId::none|string(),
         deviceClass::none|string(),
         roomId::none|string(),
         description::none|string(),
         tag::none|#tagNode{},
         version::none|integer(),
         lastUpdateTime::none|string(),
         errors::none|#errors{}
        }).

-record(deviceClass,
        {anyAttrs :: any(),
         id::string(),
         name::string()
        }).

-record(findAllDeviceClassesResponse,
        {anyAttrs :: any(),
         deviceClass::none|nonempty_list(#deviceClass{}),
         errors::none|#errors{}
        }).

-record(sessionAccounting,
        {anyAttrs :: any(),
         sessionAccountingId::string(),
         roomId::string(),
         startDay::string(),
         finishDay::none|string(),
         dateFirstUse::none|string(),
         disabled::none|boolean()
        }).

-record(guest,
        {anyAttrs :: any(),
         name::string(),
         language::none|string()
        }).

-record(pins,
        {anyAttrs :: any(),
         parentalControlPIN::none|string(),
         purchasePIN::none|string(),
         encParentalControlPIN::none|string(),
         encPurchasePIN::none|string()
        }).

-record(accountRemoteAccess,
        {anyAttrs :: any(),
         loginName::string(),
         password::none|string(),
         encPassword::none|string()
        }).

-record(accessChannel,
        {anyAttrs :: any(),
         accessType::none|nonempty_list(string())
        }).

-record(credit,
        {anyAttrs :: any(),
         amount::float()
        }).

-record(boolean,
        {anyAttrs :: any(),
         value::none|boolean()
        }).

-record(accountDetailsToCreate,
        {anyAttrs :: any(),
         credit::none|#credit{},
         accountName::string(),
         startDay::none|string(),
         finishDay::none|string(),
         accessChannel::none|#accessChannel{},
         outputsProtection::none|#boolean{},
         pins::none|#pins{},
         parentalControlLevel::none|string(),
         accountRemoteAccess::none|#accountRemoteAccess{}
        }).

-record(accountsDetailsToCreate,
        {anyAttrs :: any(),
         account::none|nonempty_list(#accountDetailsToCreate{})
        }).

-record(createSessionParams,
        {anyAttrs :: any(),
         sessionAccounting::#sessionAccounting{},
         guest::#guest{},
         accounts::none|#accountsDetailsToCreate{}
        }).

-record(accountDetails,
        {anyAttrs :: any(),
         accountId::integer(),
         sessionAccountingId::string(),
         credit::none|#credit{},
         accountName::string(),
         startDay::none|string(),
         finishDay::none|string(),
         accessChannel::none|#accessChannel{},
         outputsProtection::none|#boolean{},
         pins::none|#pins{},
         parentalControlLevel::none|string(),
         accountRemoteAccess::none|#accountRemoteAccess{}
        }).

-record(accountsDetails,
        {anyAttrs :: any(),
         account::none|nonempty_list(#accountDetails{})
        }).

-record(createSessionResponse,
        {anyAttrs :: any(),
         sessionAccounting::none|#sessionAccounting{},
         guest::none|#guest{},
         accounts::none|#accountsDetails{},
         errors::none|#errors{}
        }).

-record(findSessionByIdParams,
        {anyAttrs :: any(),
         sessionId::string()
        }).

-record(findSessionByIdResponse,
        {anyAttrs :: any(),
         sessionAccounting::none|#sessionAccounting{},
         guest::none|#guest{},
         accounts::none|#accountsDetails{},
         errors::none|#errors{}
        }).

-record(findSessionsByRoomParams,
        {anyAttrs :: any(),
         roomId::string()
        }).

-record(userSession,
        {anyAttrs :: any(),
         sessionAccounting::none|#sessionAccounting{},
         guest::none|#guest{},
         accounts::none|#accountsDetails{}
        }).

-record(findSessionsByRoomResponse,
        {anyAttrs :: any(),
         session::none|nonempty_list(#userSession{}),
         errors::none|#errors{}
        }).

-record(accountDetailsToUpdate,
        {anyAttrs :: any(),
         accountId::integer(),
         credit::none|#credit{},
         accountName::string(),
         startDay::none|string(),
         finishDay::none|string(),
         accessChannel::none|#accessChannel{},
         outputsProtection::none|#boolean{},
         pins::none|#pins{},
         parentalControlLevel::none|string(),
         accountRemoteAccess::none|#accountRemoteAccess{}
        }).

-record(accountsDetailsToUpdate,
        {anyAttrs :: any(),
         account::none|nonempty_list(#accountDetailsToUpdate{})
        }).

-record(updateSessionParams,
        {anyAttrs :: any(),
         sessionAccounting::#sessionAccounting{},
         guest::#guest{},
         accounts::none|#accountsDetailsToUpdate{}
        }).

-record(updateSessionResponse,
        {anyAttrs :: any(),
         sessionAccounting::none|#sessionAccounting{},
         guest::none|#guest{},
         accounts::none|#accountsDetails{},
         errors::none|#errors{}
        }).

-record(deleteSessionParams,
        {anyAttrs :: any(),
         sessionAccountingId::none|nonempty_list(string())
        }).

-record(deletedSessionAccounting,
        {anyAttrs :: any(),
         sessionAccountingId::string()
        }).

-record(deletedUserSession,
        {anyAttrs :: any(),
         sessionAccounting::none|#deletedSessionAccounting{}
        }).

-record(deleteSessionResponse,
        {anyAttrs :: any(),
         session::none|nonempty_list(#deletedUserSession{}),
         errors::none|#errors{}
        }).

-record(createAccountParams,
        {anyAttrs :: any(),
         sessionAccountingId::string(),
         credit::none|#credit{},
         accountName::string(),
         finishDay::none|#tagNode{},
         accessChannel::none|#accessChannel{}
        }).

-record(createAccountResponse,
        {anyAttrs :: any(),
         accountId::integer(),
         sessionAccountingId::string(),
         credit::none|#credit{},
         accountName::string(),
         startDay::string(),
         finishDay::none|#tagNode{},
         accessChannel::none|#accessChannel{},
         errors::none|#errors{}
        }).

-record(findAccountsBySessionAccountingParams,
        {anyAttrs :: any(),
         sessionAccountingId::string()
        }).

-record(account,
        {anyAttrs :: any(),
         accountId::integer(),
         sessionAccountingId::string(),
         credit::none|#credit{},
         accountName::string(),
         startDay::none|string(),
         finishDay::none|string(),
         accessChannel::none|#accessChannel{}
        }).

-record(findAccountsBySessionAccountingResponse,
        {anyAttrs :: any(),
         account::none|nonempty_list(#account{}),
         errors::none|#errors{}
        }).

-record(updateAccountParams,
        {anyAttrs :: any(),
         accountId::integer(),
         sessionAccountingId::string(),
         credit::none|#credit{},
         accountName::string(),
         startDay::string(),
         finishDay::none|#tagNode{},
         accessChannel::none|#accessChannel{}
        }).

-record(updateAccountResponse,
        {anyAttrs :: any(),
         accountId::integer(),
         sessionAccountingId::string(),
         credit::none|#credit{},
         accountName::string(),
         startDay::string(),
         finishDay::none|#tagNode{},
         accessChannel::none|#accessChannel{},
         errors::none|#errors{}
        }).

-record(deleteAccountParams,
        {anyAttrs :: any(),
         accountId::none|nonempty_list(integer())
        }).

-record(deletedAccount,
        {anyAttrs :: any(),
         accountId::integer()
        }).

-record(deleteAccountResponse,
        {anyAttrs :: any(),
         account::none|nonempty_list(#deletedAccount{}),
         errors::none|#errors{}
        }).

-record(createAccountDetailsParams,
        {anyAttrs :: any(),
         sessionAccountingId::string(),
         credit::none|#credit{},
         accountName::string(),
         finishDay::none|#tagNode{},
         accessChannel::none|#accessChannel{},
         outputsProtection::none|boolean(),
         parentalControlLevel::none|string(),
         accountRemoteAccess::none|#accountRemoteAccess{}
        }).

-record(createAccountDetailsResponse,
        {anyAttrs :: any(),
         accountId::integer(),
         sessionAccountingId::string(),
         credit::none|#credit{},
         accountName::string(),
         startDay::string(),
         finishDay::none|#tagNode{},
         accessChannel::none|#accessChannel{},
         outputsProtection::none|boolean(),
         parentalControlLevel::none|string(),
         accountRemoteAccess::none|#accountRemoteAccess{},
         errors::none|#errors{}
        }).

-record(findAccountDetailsByIdParams,
        {anyAttrs :: any(),
         accountId::integer()
        }).

-record(findAccountDetailsByIdResponse,
        {anyAttrs :: any(),
         accountId::integer(),
         sessionAccountingId::string(),
         credit::none|#credit{},
         accountName::string(),
         startDay::string(),
         finishDay::none|#tagNode{},
         accessChannel::none|#accessChannel{},
         outputsProtection::none|boolean(),
         parentalControlLevel::none|string(),
         accountRemoteAccess::none|#accountRemoteAccess{},
         errors::none|#errors{}
        }).

-record(findAccountsDetailsBySessionAccountingParams,
        {anyAttrs :: any(),
         sessionAccountingId::string()
        }).

-record(findAccountsDetailsBySessionAccountingResponse,
        {anyAttrs :: any(),
         account::none|nonempty_list(#accountDetails{}),
         errors::none|#errors{}
        }).

-record(updateAccountDetailsParams,
        {anyAttrs :: any(),
         accountId::integer(),
         sessionAccountingId::string(),
         credit::none|#credit{},
         accountName::string(),
         startDay::string(),
         finishDay::none|#tagNode{},
         accessChannel::none|#accessChannel{},
         outputsProtection::none|boolean(),
         parentalControlLevel::none|string(),
         accountRemoteAccess::none|#accountRemoteAccess{}
        }).

-record(updateAccountDetailsResponse,
        {anyAttrs :: any(),
         accountId::integer(),
         sessionAccountingId::string(),
         credit::none|#credit{},
         accountName::string(),
         startDay::string(),
         finishDay::none|#tagNode{},
         accessChannel::none|#accessChannel{},
         outputsProtection::none|boolean(),
         parentalControlLevel::none|string(),
         accountRemoteAccess::none|#accountRemoteAccess{},
         errors::none|#errors{}
        }).

-record(devicesType,
        {anyAttrs :: any(),
         device::none|nonempty_list(#deviceType{})
        }).

-record(userSessionToCreate,
        {anyAttrs :: any(),
         sessionAccounting::none|#sessionAccounting{},
         guest::none|#guest{},
         accounts::none|#accountsDetailsToCreate{}
        }).

-record(createRoomDeviceSessionParams,
        {anyAttrs :: any(),
         room::#roomType{},
         devices::none|#devicesType{},
         currentSession::none|#userSessionToCreate{}
        }).

-record(createRoomDeviceSessionResponse,
        {anyAttrs :: any(),
         room::none|#roomType{},
         devices::none|#devicesType{},
         currentSession::none|#userSession{},
         errors::none|#errors{}
        }).

-record(findRoomDeviceSessionByIdParams,
        {anyAttrs :: any(),
         roomId::string()
        }).

-record(findRoomDeviceSessionByIdResponse,
        {anyAttrs :: any(),
         room::none|#roomType{},
         devices::none|#devicesType{},
         currentSession::none|#userSession{},
         errors::none|#errors{}
        }).

-record(findRoomDeviceSessionsParams,
        {anyAttrs :: any(),
         startIndex::integer(),
         count::integer(),
         sortBy::none|string(),
         order::none|string(),
         'query'::none|string()
        }).

-record(deviceToUpdate,
        {anyAttrs :: any(),
         id::integer(),
         physicalId::string(),
         deviceClass::none|string(),
         roomId::none|string(),
         description::none|string(),
         tag::none|#tagNode{},
         version::none|integer(),
         lastUpdateTime::none|string()
        }).

-record(devicesToUpdate,
        {anyAttrs :: any(),
         device::none|nonempty_list(#deviceToUpdate{})
        }).

-record(roomDeviceSession,
        {anyAttrs :: any(),
         room::#roomType{},
         devices::none|#devicesToUpdate{},
         currentSession::none|#userSession{}
        }).

-record(findRoomDeviceSessionsResponse,
        {anyAttrs :: any(),
         existsMore::boolean(),
         countTotal::integer(),
         roomDeviceSession::none|nonempty_list(#roomDeviceSession{}),
         errors::none|#errors{}
        }).

-record(findRoomDeviceSessionsByTagIdParams,
        {anyAttrs :: any(),
         startIndex::integer(),
         count::integer(),
         sortBy::none|string(),
         order::none|string(),
         'query'::none|string(),
         tagId::integer()
        }).

-record(findRoomDeviceSessionsByTagIdResponse,
        {anyAttrs :: any(),
         existsMore::boolean(),
         countTotal::integer(),
         roomDeviceSession::none|nonempty_list(#roomDeviceSession{}),
         errors::none|#errors{}
        }).

-record(updateRoomDeviceSessionParams,
        {anyAttrs :: any(),
         room::#roomType{},
         devices::none|#devicesToUpdate{},
         currentSession::none|#userSession{}
        }).

-record(userSessionToUpdate,
        {anyAttrs :: any(),
         sessionAccounting::none|#sessionAccounting{},
         guest::none|#guest{},
         accounts::none|#accountsDetailsToUpdate{}
        }).

-record(updateRoomDeviceSessionResponse,
        {anyAttrs :: any(),
         room::none|#roomType{},
         devices::none|#devicesType{},
         currentSession::none|#userSessionToUpdate{},
         errors::none|#errors{}
        }).

-record(deleteRoomDeviceSessionParams,
        {anyAttrs :: any(),
         roomId::none|nonempty_list(string())
        }).

-record(deletedRoom,
        {anyAttrs :: any(),
         roomId::string()
        }).

-record(deletedRoomDeviceSession,
        {anyAttrs :: any(),
         room::#deletedRoom{}
        }).

-record(deleteRoomDeviceSessionResponse,
        {anyAttrs :: any(),
         roomDeviceSession::none|nonempty_list(#deletedRoomDeviceSession{}),
         errors::none|#errors{}
        }).

-record(networkAreaType,
        {anyAttrs :: any(),
         id::none|integer(),
         name::none|string()
        }).

-record(videoServer,
        {anyAttrs :: any(),
         id::none|integer(),
         name::none|string(),
         publicIp::none|string(),
         publicPort::none|integer(),
         internalIp::none|string(),
         internalPort::none|integer(),
         maxRequests::none|integer(),
         maxInactivitySeconds::none|integer(),
         videoServerType::none|string(),
         videoServerContent::none|string(),
         networkArea::none|#networkAreaType{},
         active::none|boolean(),
         errors::none|#errors{}
        }).

-record(findVideoServerById,
        {anyAttrs :: any(),
         videoServerId::integer()
        }).

-record(videoServerType,
        {anyAttrs :: any(),
         id::none|integer(),
         name::none|string(),
         publicIp::none|string(),
         publicPort::none|integer(),
         internalIp::none|string(),
         internalPort::none|integer(),
         maxRequests::none|integer(),
         maxInactivitySeconds::none|integer(),
         videoServerType::none|string(),
         videoServerContent::none|string(),
         networkArea::none|#networkAreaType{},
         active::none|boolean()
        }).

-record(videoServers,
        {anyAttrs :: any(),
         videoServer::none|nonempty_list(#videoServerType{}),
         errors::none|#errors{}
        }).

-record(deleteVideoServer,
        {anyAttrs :: any(),
         videoServerId::nonempty_list(integer())
        }).

-record(videoServerStreamType,
        {anyAttrs :: any(),
         name::string(),
         bitRate::none|integer(),
         running::boolean()
        }).

-record(videoServerStreamsType,
        {anyAttrs :: any(),
         stream::none|nonempty_list(#videoServerStreamType{})
        }).

-record(videoServerStatus,
        {anyAttrs :: any(),
         id::none|integer(),
         videoServerId::none|integer(),
         name::none|string(),
         appName::none|string(),
         connectedClients::none|integer(),
         lastUpdate::none|string(),
         streams::none|#videoServerStreamsType{},
         errors::none|#errors{}
        }).

-record(findVideoServerStatusById,
        {anyAttrs :: any(),
         videoServerStatusId::integer()
        }).

-record(videoServerStatusType,
        {anyAttrs :: any(),
         id::none|integer(),
         videoServerId::none|integer(),
         name::none|string(),
         appName::none|string(),
         connectedClients::none|integer(),
         lastUpdate::none|string(),
         streams::none|#videoServerStreamsType{}
        }).

-record(videoServersStatus,
        {anyAttrs :: any(),
         videoServerStatus::none|nonempty_list(#videoServerStatusType{}),
         errors::none|#errors{}
        }).

-record(deleteVideoServerStatus,
        {anyAttrs :: any(),
         videoServerStatusId::nonempty_list(integer())
        }).

-record(videoServersType,
        {anyAttrs :: any(),
         videoServer::none|nonempty_list(#videoServerType{})
        }).

-record(videoServerGroup,
        {anyAttrs :: any(),
         id::none|integer(),
         appName::none|string(),
         type::none|string(),
         videoServers::none|#videoServersType{},
         errors::none|#errors{}
        }).

-record(findVideoServerGroupById,
        {anyAttrs :: any(),
         videoServerGroupId::integer()
        }).

-record(findVideoServerGroupByType,
        {anyAttrs :: any(),
         videoServerGroupType::string()
        }).

-record(videoServerGroupType,
        {anyAttrs :: any(),
         id::none|integer(),
         appName::none|string(),
         type::none|string(),
         videoServers::none|#videoServersType{}
        }).

-record(videoServerGroups,
        {anyAttrs :: any(),
         videoServerGroup::none|nonempty_list(#videoServerGroupType{}),
         errors::none|#errors{}
        }).

-record(deleteVideoServerGroup,
        {anyAttrs :: any(),
         videoServerGroupId::nonempty_list(integer())
        }).

-record('I18nType/i18nValues/i18nValue',
        {anyAttrs :: any(),
         lang::string(),
         choice::none|string()
        }).

-record('I18nType/i18nValues',
        {anyAttrs :: any(),
         i18nValue::none|nonempty_list(#'I18nType/i18nValues/i18nValue'{})
        }).

-record('I18nType',
        {anyAttrs :: any(),
         i18nId::none|integer(),
         i18nValues::#'I18nType/i18nValues'{}
        }).

-record(screenSaver,
        {anyAttrs :: any(),
         id::none|integer(),
         name::none|string(),
         version::none|string(),
         url::none|string(),
         nameI18n::none|#'I18nType'{},
         errors::none|#errors{}
        }).

-record(findScreenSaversParams,
        {anyAttrs :: any(),
         startIndex::integer(),
         count::integer(),
         sortBy::none|string(),
         order::none|string(),
         'query'::none|string()
        }).

-record(screenSaverType,
        {anyAttrs :: any(),
         id::none|integer(),
         name::none|string(),
         version::none|string(),
         url::none|string(),
         nameI18n::none|#'I18nType'{}
        }).

-record(screenSavers,
        {anyAttrs :: any(),
         existsMore::boolean(),
         countTotal::integer(),
         screenSaver::none|nonempty_list(#screenSaverType{}),
         errors::none|#errors{}
        }).

-record(findScreenSaverByIdParams,
        {anyAttrs :: any(),
         screenSaverId::integer()
        }).

-record(deleteScreenSaverParams,
        {anyAttrs :: any(),
         screenSaverId::none|nonempty_list(integer())
        }).

-record(externalNetworksConfType,
        {anyAttrs :: any(),
         channelUrlResponseType::string(),
         vodkatvChannelId::none|string()
        }).

-record(networkingConfigurationType,
        {anyAttrs :: any(),
         externalNetworksConf::#externalNetworksConfType{}
        }).

-record(networkingConfiguration,
        {anyAttrs :: any(),
         externalNetworksConf::#externalNetworksConfType{},
         errors::none|#errors{}
        }).

-record(listConfigurationAssetType,
        {anyAttrs :: any(),
         listConfigurationAssetId::none|integer(),
         assetId::none|string(),
         position::none|integer(),
         title::none|string()
        }).

-record(listConfigurationAssetsType,
        {anyAttrs :: any(),
         listConfigurationAsset::none|nonempty_list(#listConfigurationAssetType{})
        }).

-record(listConfiguration,
        {anyAttrs :: any(),
         listConfigurationId::none|integer(),
         name::none|string(),
         description::none|string(),
         count::none|integer(),
         assetCollectionId::none|string(),
         sortBy::none|string(),
         order::none|string(),
         newAssetsPosition::none|string(),
         sortByNewAssetsPosition::none|string(),
         orderNewAssetsPosition::none|string(),
         listConfigurationAssets::none|#listConfigurationAssetsType{},
         errors::none|#errors{}
        }).

-record(findListConfigurationsParams,
        {anyAttrs :: any(),
         startIndex::integer(),
         count::integer(),
         sortBy::none|string(),
         order::none|string(),
         'query'::none|string()
        }).

-record(listConfigurationType,
        {anyAttrs :: any(),
         listConfigurationId::none|integer(),
         name::none|string(),
         description::none|string(),
         count::none|integer(),
         assetCollectionId::none|string(),
         sortBy::none|string(),
         order::none|string(),
         newAssetsPosition::none|string(),
         sortByNewAssetsPosition::none|string(),
         orderNewAssetsPosition::none|string(),
         listConfigurationAssets::none|#listConfigurationAssetsType{}
        }).

-record(listConfigurations,
        {anyAttrs :: any(),
         existsMore::boolean(),
         countTotal::integer(),
         listConfiguration::none|nonempty_list(#listConfigurationType{}),
         errors::none|#errors{}
        }).

-record(findListConfigurationByIdParams,
        {anyAttrs :: any(),
         listConfigurationId::integer()
        }).

-record(deleteListConfigurationParams,
        {anyAttrs :: any(),
         listConfigurationId::none|nonempty_list(integer())
        }).

-record(listConfigurationAssets,
        {anyAttrs :: any(),
         existsMore::boolean(),
         countTotal::integer(),
         listConfigurationAsset::none|nonempty_list(#listConfigurationAssetType{}),
         errors::none|#errors{}
        }).

-record(filterListConfiguration,
        {anyAttrs :: any(),
         filterListConfigurationId::none|integer(),
         listConfigurationId::none|integer(),
         count::none|integer(),
         assetCollectionId::none|string(),
         errors::none|#errors{}
        }).

-record(findFilterListConfigurationsParams,
        {anyAttrs :: any(),
         listConfigurationId::integer()
        }).

-record(filterListConfigurationType,
        {anyAttrs :: any(),
         filterListConfigurationId::none|integer(),
         listConfigurationId::none|integer(),
         count::none|integer(),
         assetCollectionId::none|string()
        }).

-record(filterListConfigurations,
        {anyAttrs :: any(),
         existsMore::boolean(),
         countTotal::integer(),
         filterListConfiguration::none|nonempty_list(#filterListConfigurationType{}),
         errors::none|#errors{}
        }).

-record(deleteFilterListConfigurationParams,
        {anyAttrs :: any(),
         filterListConfigurationId::none|nonempty_list(integer())
        }).

-record(textChannelType,
        {anyAttrs :: any(),
         txtChannelId::none|string(),
         name::none|string(),
         errors::none|#errors{}
        }).

-record(parentalRatingType,
        {anyAttrs :: any(),
         parentalRatingId::none|string(),
         value::none|string(),
         position::none|integer(),
         errors::none|#errors{}
        }).

-record(gapsType,
        {anyAttrs :: any(),
         startTime::none|integer(),
         endTime::none|integer()
        }).

-record(pauseType,
        {anyAttrs :: any(),
         supported::none|pauseSupportedType(),
         gaps::none|#gapsType{}
        }).

-record(startOverType,
        {anyAttrs :: any(),
         supported::none|startOverSupportedType(),
         preRollAssetId::none|string(),
         gaps::none|#gapsType{},
         defaultTimes::none|#gapsType{}
        }).

-record(netPVRType,
        {anyAttrs :: any(),
         fragmentSize::none|integer(),
         recordTime::none|integer(),
         pause::none|#pauseType{},
         startOver::none|#startOverType{}
        }).

-record(pvrType,
        {anyAttrs :: any(),
         supported::none|boolean()
        }).

-record(localPVRType,
        {anyAttrs :: any(),
         supported::none|boolean()
        }).

-record(epgType,
        {anyAttrs :: any(),
         availableHours::none|integer()
        }).

-record(contentCategoryType,
        {anyAttrs :: any(),
         categoryId::integer(),
         categoryName::none|string(),
         errors::none|#errors{}
        }).

-record(contentCategoriesType,
        {anyAttrs :: any(),
         contentCategory::none|nonempty_list(#contentCategoryType{}),
         errors::none|#errors{}
        }).

-record(videoServerGroupsType,
        {anyAttrs :: any(),
         videoServerGroup::none|nonempty_list(#videoServerGroupType{})
        }).

-record(deviceClassType,
        {anyAttrs :: any(),
         id::none|string(),
         name::none|string(),
         errors::none|#errors{}
        }).

-record(deviceTypeType,
        {anyAttrs :: any(),
         deviceTypeId::none|integer(),
         userAgent::none|string(),
         name::none|string(),
         deviceClass::none|#deviceClassType{},
         errors::none|#errors{}
        }).

-record(channelVideoPidType,
        {anyAttrs :: any(),
         channelVideoPidId::none|integer(),
         channelDataPlayInfoId::none|integer(),
         codec::none|string(),
         pid::none|integer()
        }).

-record(channelVideoPidsType,
        {anyAttrs :: any(),
         channelVideoPid::none|nonempty_list(#channelVideoPidType{}),
         errors::none|#errors{}
        }).

-record(channelAudioPidType,
        {anyAttrs :: any(),
         channelAudioPidId::none|integer(),
         channelDataPlayInfoId::none|integer(),
         language::none|string(),
         codec::none|string(),
         pid::none|integer()
        }).

-record(channelAudioPidsType,
        {anyAttrs :: any(),
         channelAudioPid::none|nonempty_list(#channelAudioPidType{}),
         errors::none|#errors{}
        }).

-record(channelSourceURIType,
        {anyAttrs :: any(),
         channelSourceURIId::none|integer(),
         sourceURI::none|string(),
         bitRate::none|integer(),
         channelDataPlayInfoId::none|integer()
        }).

-record(channelSourceURIsType,
        {anyAttrs :: any(),
         channelSourceURI::none|nonempty_list(#channelSourceURIType{}),
         errors::none|#errors{}
        }).

-record(channelDataPlayInfoType,
        {anyAttrs :: any(),
         channelDataPlayInfoId::none|integer(),
         channelUrl::none|string(),
         aclChannelId::none|string(),
         aclChannelProtocol::none|string(),
         preBufferMillis::none|integer(),
         netPVRUrl::none|string(),
         netPVRChannelName::none|string(),
         preBufferNetPVRMillis::none|integer(),
         position::none|integer(),
         deviceType::none|#deviceTypeType{},
         channelVideoPids::none|#channelVideoPidsType{},
         channelAudioPids::none|#channelAudioPidsType{},
         channelSourceURIs::none|#channelSourceURIsType{},
         streamName::none|string(),
         videoServerGroupsOrigin::none|#videoServerGroupsType{},
         videoServerGroupsEdge::none|#videoServerGroupsType{}
        }).

-record(channelDataPlayInfosType,
        {anyAttrs :: any(),
         channelDataPlayInfo::none|nonempty_list(#channelDataPlayInfoType{}),
         errors::none|#errors{}
        }).

-record(channel,
        {anyAttrs :: any(),
         vodkatvChannelId::none|string(),
         bahamasChannelId::none|string(),
         channelType::none|channelTypeType(),
         accessType::none|string(),
         name::none|string(),
         description::none|string(),
         logoURL::none|string(),
         languageCode::none|string(),
         countryCode::none|string(),
         parentalRating::none|#parentalRatingType{},
         active::none|boolean(),
         casChannelId::none|string(),
         teletext::none|#textChannelType{},
         netPVR::none|#netPVRType{},
         pvr::none|#pvrType{},
         localPVR::none|#localPVRType{},
         contentCategories::none|#contentCategoriesType{},
         epg::none|#epgType{},
         channelDataPlayInfos::none|#channelDataPlayInfosType{},
         errors::none|#errors{}
        }).

-record(findVodkatvChannelsParams,
        {anyAttrs :: any(),
         startIndex::integer(),
         count::integer(),
         sortBy::none|string(),
         order::none|string(),
         'query'::none|string()
        }).

-record(channelType,
        {anyAttrs :: any(),
         vodkatvChannelId::none|string(),
         bahamasChannelId::none|string(),
         channelType::none|channelTypeType(),
         accessType::none|string(),
         name::none|string(),
         description::none|string(),
         logoURL::none|string(),
         languageCode::none|string(),
         countryCode::none|string(),
         parentalRating::none|#parentalRatingType{},
         active::none|boolean(),
         casChannelId::none|string(),
         teletext::none|#textChannelType{},
         netPVR::none|#netPVRType{},
         pvr::none|#pvrType{},
         localPVR::none|#localPVRType{},
         contentCategories::none|#contentCategoriesType{},
         epg::none|#epgType{},
         channelDataPlayInfos::none|#channelDataPlayInfosType{}
        }).

-record(channels,
        {anyAttrs :: any(),
         existsMore::boolean(),
         countTotal::integer(),
         channel::none|nonempty_list(#channelType{}),
         errors::none|#errors{}
        }).

-record(findChannelByIdParams,
        {anyAttrs :: any(),
         vodkatvChannelId::string()
        }).

-record(findVodkatvChannelsByAccountParams,
        {anyAttrs :: any(),
         accountId::integer(),
         deviceId::integer(),
         channelType::string()
        }).

-record(deleteVodkatvChannelParams,
        {anyAttrs :: any(),
         vodkatvChannelId::none|nonempty_list(string())
        }).

-record(casChannelType,
        {anyAttrs :: any(),
         id::none|string(),
         name::none|string()
        }).

-record(casChannels,
        {anyAttrs :: any(),
         casChannel::none|nonempty_list(#casChannelType{}),
         errors::none|#errors{}
        }).

-record(roleType,
        {anyAttrs :: any(),
         name::none|string()
        }).

-record(userType,
        {anyAttrs :: any(),
         name::none|string()
        }).

-record(usersType,
        {anyAttrs :: any(),
         user::none|nonempty_list(#userType{})
        }).

-record(roleUsersType,
        {anyAttrs :: any(),
         role::none|#roleType{},
         users::none|#usersType{}
        }).

-record(rolesUsers,
        {anyAttrs :: any(),
         existsMore::boolean(),
         countTotal::integer(),
         roleUsers::none|nonempty_list(#roleUsersType{}),
         errors::none|#errors{}
        }).

-record(roleUsers,
        {anyAttrs :: any(),
         role::none|#roleType{},
         users::none|#usersType{},
         errors::none|#errors{}
        }).

-record(findRoleUsersByRoleName,
        {anyAttrs :: any(),
         roleName::string()
        }).

-record(deleteRoleUsers,
        {anyAttrs :: any(),
         roleName::nonempty_list(string())
        }).

-record(menuOptionsType,
        {anyAttrs :: any(),
         menuOption::none|nonempty_list(string())
        }).

-record(roleMenuOptionsType,
        {anyAttrs :: any(),
         role::none|#roleType{},
         menuOptions::none|#menuOptionsType{}
        }).

-record(rolesMenuOptions,
        {anyAttrs :: any(),
         existsMore::boolean(),
         countTotal::integer(),
         roleMenuOptions::none|nonempty_list(#roleMenuOptionsType{}),
         errors::none|#errors{}
        }).

-record(roleMenuOptions,
        {anyAttrs :: any(),
         role::none|#roleType{},
         menuOptions::none|#menuOptionsType{},
         errors::none|#errors{}
        }).

-record(findRoleMenuOptionsByRoleName,
        {anyAttrs :: any(),
         roleName::string()
        }).

-record(deleteRoleMenuOptions,
        {anyAttrs :: any(),
         roleName::nonempty_list(string())
        }).

-record(users,
        {anyAttrs :: any(),
         user::none|nonempty_list(#userType{}),
         errors::none|#errors{}
        }).

-record(menuOptions,
        {anyAttrs :: any(),
         menuOption::none|nonempty_list(string()),
         errors::none|#errors{}
        }).

-record(roomDeviceSessionToCreate,
        {anyAttrs :: any(),
         room::#roomType{},
         devices::none|#devicesToUpdate{},
         currentSession::none|#userSessionToCreate{}
        }).

-record(roomDeviceSessionToUpdate,
        {anyAttrs :: any(),
         room::#roomType{},
         devices::none|#devicesToUpdate{},
         currentSession::none|#userSessionToUpdate{}
        }).

-record(dummyParams,
        {anyAttrs :: any(),
         userSessionToCreate::none|nonempty_list(#userSessionToCreate{}),  %% %% MaxOccurs:0
         userSessionToUpdate::none|nonempty_list(#userSessionToUpdate{}),  %% %% MaxOccurs:0
         roomDeviceSessionToCreate::none|nonempty_list(#roomDeviceSessionToCreate{}),  %% %% MaxOccurs:0
         roomDeviceSessionToUpdate::none|nonempty_list(#roomDeviceSessionToUpdate{})  %% %% MaxOccurs:0
        }).

-record(dummyResponse,
        {anyAttrs :: any()}).

-record(accountToCreate,
        {anyAttrs :: any(),
         credit::none|#credit{},
         accountName::string(),
         startDay::none|string(),
         finishDay::none|string(),
         accessChannel::none|#accessChannel{}
        }).

-record(accountToUpdate,
        {anyAttrs :: any(),
         accountId::integer(),
         credit::none|#credit{},
         accountName::string(),
         startDay::none|string(),
         finishDay::none|string(),
         accessChannel::none|#accessChannel{}
        }).

-record(videoServersStatusType,
        {anyAttrs :: any(),
         videoServerStatus::none|nonempty_list(#videoServerStatusType{})
        }).

-record(screenSaversType,
        {anyAttrs :: any(),
         screenSaver::none|nonempty_list(#screenSaverType{})
        }).

-record(auxCacheServerType,
        {anyAttrs :: any(),
         host::string(),
         port::integer()
        }).

-record(updateAuxCacheServersParams,
        {anyAttrs :: any(),
         auxCacheServer::none|nonempty_list(#auxCacheServerType{})
        }).

-record(auxCacheServers,
        {anyAttrs :: any(),
         auxCacheServer::none|nonempty_list(#auxCacheServerType{}),
         errors::none|#errors{}
        }).

-record(listConfigurationsType,
        {anyAttrs :: any(),
         listConfiguration::none|nonempty_list(#listConfigurationType{})
        }).

-record(filterListConfigurationsType,
        {anyAttrs :: any(),
         filterListConfiguration::none|nonempty_list(#filterListConfigurationType{})
        }).

-record(channelsType,
        {anyAttrs :: any(),
         channel::none|nonempty_list(#channelType{})
        }).

-type pauseSupportedType()::'NOT_ALLOW'| 'ONLY_FORWARD'.

-type startOverSupportedType()::'DEACTIVATE'| 'FREE_FOR_ALL'.

-type channelTypeType()::'VIDEO'| 'AUDIO'.

-record(casChannelsType,
        {anyAttrs :: any(),
         casChannel::none|nonempty_list(#casChannelType{})
        }).

-record(rolesUsersType,
        {anyAttrs :: any(),
         roleUsers::none|nonempty_list(#roleUsersType{})
        }).

-record(rolesMenuOptionsType,
        {anyAttrs :: any(),
         roleMenuOptions::none|nonempty_list(#roleMenuOptionsType{})
        }).

