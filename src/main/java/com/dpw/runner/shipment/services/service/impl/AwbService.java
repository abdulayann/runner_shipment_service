package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.adapters.impl.BridgeServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerPartialListResponse;
import com.dpw.runner.shipment.services.config.SyncConfig;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.request.awb.*;
import com.dpw.runner.shipment.services.dto.request.bridgeService.TactBridgePayload;
import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.dto.response.bridgeService.BridgeServiceResponse;
import com.dpw.runner.shipment.services.dto.v1.request.V1RetrieveRequest;
import com.dpw.runner.shipment.services.dto.v1.response.OrgAddressResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1RetrieveResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.*;
import com.dpw.runner.shipment.services.entitytransfer.dto.*;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.masterdata.dto.MasterData;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequest;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequestV2;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.service.interfaces.*;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.dpw.runner.shipment.services.syncing.Entity.SaveStatus;
import com.dpw.runner.shipment.services.syncing.constants.SyncingConstants;
import com.dpw.runner.shipment.services.syncing.interfaces.IAwbSync;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSync;
import com.dpw.runner.shipment.services.utils.*;
import com.dpw.runner.shipment.services.validator.constants.ErrorConstants;
import com.dpw.runner.shipment.services.validator.enums.Operators;
import com.google.common.base.Strings;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.retry.support.RetryTemplate;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.client.RestTemplate;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.*;
import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.IsStringNullOrEmpty;
import static com.dpw.runner.shipment.services.utils.CommonUtils.stringValueOf;

@SuppressWarnings("ALL")
@Service
@Slf4j
public class AwbService implements IAwbService {

    @Autowired
    private IConsolidationService consolidationService;

    @Autowired
    private IAwbDao awbDao;

    @Autowired
    IShipmentDao shipmentDao;

    @Autowired
    IConsolidationDetailsDao consolidationDetailsDao;

    @Autowired
    IPackingDao packingDao;

    @Autowired
    IMawbHawbLinkDao mawbHawbLinkDao;

    @Autowired
    IConsoleShipmentMappingDao consoleShipmentMappingDao;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private ModelMapper modelMapper;

    @Autowired
    private UserContext userContext;

    @Autowired
    private IAuditLogService auditLogService;

    @Autowired
    private RetryTemplate retryTemplate;

    @Autowired
    RestTemplate restTemplate;

    @Autowired
    private IShipmentSettingsDao shipmentSettingsDao;

    @Autowired
    private UnitConversionUtility unitConversionUtility;

    @Autowired
    private SyncConfig syncConfig;

    @Autowired
    IAwbSync awbSync;

    @Autowired
    IShipmentService shipmentService;

    @Autowired
    IShipmentSync shipmentSync;

    @Autowired
    private IV1Service v1Service;

    @Autowired
    private MasterDataUtils masterDataUtils;
    @Autowired
    private BridgeServiceAdapter bridgeServiceAdapter;

    @Autowired
    private MasterDataKeyUtils masterDataKeyUtils;
    @Autowired
    private IAirMessagingLogsService airMessagingLogsService;

    @Autowired
    ExecutorService executorService;

    @Autowired
    private PartialFetchUtils partialFetchUtils;

    @Autowired
    private CommonUtils commonUtils;

    @Autowired
    private V1ServiceUtil v1ServiceUtil;

    private static final String errorMessage = "You cannot generate the AWB without adding the screening/ Security status for RA KC shipments";

    private Integer totalPacks = 0;
    private List<String> attachedShipmentDescriptions = new ArrayList<>();
    private BigDecimal totalVolumetricWeightOfAwbPacks = new BigDecimal(0);

    @Value("${v1service.url.base}${v1service.url.awbSync}")
    private String AWB_V1_SYNC_URL;

    private String iataCode;
    private String executedAt;

    @Transactional
    public ResponseEntity<IRunnerResponse> createAwb(CommonRequestModel commonRequestModel) {
        String responseMsg;
        CreateAwbRequest request = (CreateAwbRequest) commonRequestModel.getData();
        if (request == null) {
            log.debug("Request is empty for AWB Create for Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new ValidationException("Request can't be empty for creating AWB");
        }

        if (request.getShipmentId() == null) {
            log.error("Shipment Id can't be null or empty in create AWB Request");
            throw new ValidationException("Shipment Id can't be null or empty in Create AWB Request");
        }

        Awb awb = new Awb();
        try {
            awb = awbDao.save(generateAwb(request));
            try {
                callV1Sync(awb, SaveStatus.CREATE);
            } catch (Exception e) {
                log.error(SyncingConstants.ERROR_PERFORMING_AWB_SYNC, e);
            }

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(awb)
                            .prevData(null)
                            .parent(Awb.class.getSimpleName())
                            .parentId(awb.getId())
                            .operation(DBOperationType.CREATE.name()).build()
            );

            log.info("AWB created successfully for Id {} with Request Id {}", awb.getId(), LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(awb));
    }

    public ResponseEntity<IRunnerResponse> updateAwb(CommonRequestModel commonRequestModel) {
        String responseMsg;
        AwbRequest request = (AwbRequest) commonRequestModel.getData();

        Awb awb = convertRequestToEntity(request);
        awb.setAwbNumber(awb.getAwbShipmentInfo().getAwbNumber());

        try {
            if (request.getId() == null) {
                log.error("Request Id is null for AWB update for Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new RunnerException("Request Id can't be null");
            }
            long id = request.getId();
            Optional<Awb> oldEntity = awbDao.findById(id);
            if (!oldEntity.isPresent()) {
                log.debug(AwbConstants.AWB_RETRIEVE_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            this.validateAwbBeforeUpdate(awb);

            String oldEntityJsonString = jsonHelper.convertToJson(oldEntity.get());
            updateAwbOtherChargesInfo(awb.getAwbOtherChargesInfo());
            if(awb.getAwbShipmentInfo().getEntityType().equals(Constants.MAWB)) {
                List<AwbPackingInfo> awbPackingInfoList = awb.getAwbPackingInfo();
                awb.setAwbPackingInfo(null);
                updateAwbPacking(awb.getId(), awbPackingInfoList);
            } else {
                if(awb.getAwbPackingInfo() != null && awb.getAwbPackingInfo().size() > 0) {
                    for(var i : awb.getAwbPackingInfo()) {
                        i.setAwbNumber(awb.getAwbNumber());
                    }
                }
                awbDao.updateSciFieldFromHawb(awb, oldEntity.get(), false, id);
            }
            awb = awbDao.save(awb);

            try {
                callV1Sync(awb, SaveStatus.UPDATE);
            } catch (Exception e) {
                log.error(SyncingConstants.ERROR_PERFORMING_AWB_SYNC, e);
            }

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(awb)
                            .prevData(jsonHelper.readFromJson(oldEntityJsonString, Awb.class))
                            .parent(Awb.class.getSimpleName())
                            .parentId(awb.getId())
                            .operation(DBOperationType.UPDATE.name()).build()
            );

            log.info("Updated the AWB Shipment Info for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(awb));
    }

    private void validateAwbBeforeUpdate(Awb awb) {
        Set<String> errors = new LinkedHashSet<>();
        // Routings leg no can not be repeated
        if (awb.getAwbRoutingInfo() != null && !awb.getAwbRoutingInfo().isEmpty()) {
            HashSet<Long> hashSet = new HashSet<>();
            for (AwbRoutingInfo routings : awb.getAwbRoutingInfo()) {
                if (routings.getLeg() != null) {
                    if (hashSet.contains(routings.getLeg())) {
                        errors.add("This Flight Sequence number already exists. Please use another");
                        break;
                    } else
                        hashSet.add(routings.getLeg());
                }
            }
        }

        if(!errors.isEmpty())
            throw new ValidationException(String.join(",", errors));
    }

    private void updateAwbPacking(long mawbId, List<AwbPackingInfo> awbPackingInfoList) throws RunnerException {
        if(awbPackingInfoList == null) {
            return;
        }

        Map<String, List<AwbPackingInfo>> dataMap = new HashMap<>();

        for(AwbPackingInfo awbPackingInfo : awbPackingInfoList) {
            dataMap.putIfAbsent(awbPackingInfo.getAwbNumber(), new ArrayList<>());
            dataMap.get(awbPackingInfo.getAwbNumber()).add(awbPackingInfo);
        }

        List<Awb> awbList = getLinkedAwbFromMawb(mawbId);
        if(awbList != null) {
            commonUtils.setInterBranchContextForHub();
            for(Awb awb : awbList) {
                awb.setAwbPackingInfo(dataMap.get(awb.getAwbNumber()));
                awbDao.save(awb);
            }
        }
    }

    public ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            FetchAwbListRequest request = (FetchAwbListRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for AWB list for Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            // construct specifications for filter request
            Pair<Specification<Awb>, Pageable> tuple = fetchData(request, Awb.class);
            Page<Awb> awbPage = awbDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("AWB list retrieved successfully for Request Id {}", LoggerHelper.getRequestIdFromMDC());

            List<String>includeColumns = request.getIncludeColumns();

            List<Awb> awbList = awbPage.getContent();

            if(awbList != null) {
                ShipmentSettingsDetails tenantSettings = commonUtils.getShipmentSettingFromContext();
                for (Awb awb : awbList) {
                    if (awb.getAwbShipmentInfo().getEntityType().equals(Constants.MAWB)) {
                        if(request.getFromGenerateAwbButton() != null && request.getFromGenerateAwbButton()
                                &&  tenantSettings != null && ((tenantSettings.getRestrictAWBEdit() != null
                                && tenantSettings.getRestrictAWBEdit()) || (tenantSettings.getAutoUpdateShipmentAWB() != null && tenantSettings.getAutoUpdateShipmentAWB()))) {
                            try {
                                CreateAwbRequest req = CreateAwbRequest.builder().ConsolidationId(awb.getConsolidationId()).AwbType(awb.getAwbShipmentInfo().getEntityType()).build();
                                partialAutoUpdateMawb(CommonRequestModel.buildRequest(req));
                            } catch (Exception ex) {
                                log.error("Error while updating Mawb due to: " + ex.getMessage());
                            }
                        }

                        getMawnLinkPacks(awb);

                    } else {
                        if(request.getFromGenerateAwbButton() != null && request.getFromGenerateAwbButton()
                                &&  tenantSettings != null && ((tenantSettings.getRestrictAWBEdit() != null
                                && tenantSettings.getRestrictAWBEdit()) || (tenantSettings.getAutoUpdateShipmentAWB() != null && tenantSettings.getAutoUpdateShipmentAWB()))) {
                            try {
                                CreateAwbRequest req = CreateAwbRequest.builder().ShipmentId(awb.getShipmentId()).AwbType(awb.getAwbShipmentInfo().getEntityType()).build();
                                partialAutoUpdateAwb(CommonRequestModel.buildRequest(req));
                            } catch (Exception ex) {
                                log.error("Error while updating Hawb due to: " + ex.getMessage());
                            }
                        }
                    }
                }
            }

            if(includeColumns==null||includeColumns.size()==0){
                return ResponseHelper.buildListSuccessResponse(
                        convertEntityListToDtoList(awbList),
                        awbPage.getTotalPages(),
                        awbPage.getTotalElements());
            }
            else{

                List<IRunnerResponse>filtered_list=new ArrayList<>();
                for( var curr: convertEntityListToDtoList(awbList)){
                    RunnerPartialListResponse res=new RunnerPartialListResponse();
                    res.setData(partialFetchUtils.fetchPartialListData(curr,request.getIncludeColumns()));
                    filtered_list.add( res);
                }
                return ResponseHelper.buildListSuccessResponse(
                        filtered_list,
                        awbPage.getTotalPages(),
                        awbPage.getTotalElements());
            }

        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    //Calculate mawb packs
    @Override
    public Awb getMawnLinkPacks(Awb awb) {
        try {
            ShipmentSettingsDetails tenantSettings = commonUtils.getShipmentSettingFromContext();
            List<Awb> linkedHawb = getLinkedAwbFromMawb(awb.getId());
            List<AwbPackingInfo> linkedPacks = new ArrayList<>();
            for (var hawb : linkedHawb) {
                if(hawb.getAwbPackingInfo() != null) {
                    hawb.setTenantId(hawb.getTenantId());
                    linkedPacks.addAll(hawb.getAwbPackingInfo());
                }
            }
            awb.setAwbPackingInfo(linkedPacks);
            if(awb.getAwbGoodsDescriptionInfo() != null && awb.getAwbGoodsDescriptionInfo().size() > 0) {
                calculateGoodsDescription(awb.getAwbGoodsDescriptionInfo().get(0), linkedPacks, tenantSettings, new HashMap<>(), linkedPacks.size() > 0);
            }
        } catch (Exception e) {
            log.error(e.getMessage());
        }
        return awb;
    }

    public ResponseEntity<IRunnerResponse> retrieveById(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.error(AwbConstants.AWB_RETRIEVE_REQUEST_NULL_ERROR, LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.error("Request Id is null for AWB retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<Awb> awb = awbDao.findById(id);
            if (!awb.isPresent()) {
                log.debug(AwbConstants.AWB_RETRIEVE_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("AWB fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());

            // Get packs of all linked HAWB
            if(awb.get().getAwbShipmentInfo().getEntityType().equals(Constants.MAWB)) {
                List<ShipmentSettingsDetails> tenantSettingsList = shipmentSettingsDao.getSettingsByTenantIds(Arrays.asList(UserContext.getUser().TenantId));
                ShipmentSettingsDetails tenantSettings = null;
                if (tenantSettingsList != null && tenantSettingsList.size() >= 1) {
                    tenantSettings = tenantSettingsList.get(0);
                }
                List<Awb> linkedHawb = getLinkedAwbFromMawb(awb.get().getId());
                List<AwbPackingInfo> linkedPacks = new ArrayList<>();
                for(var hawb : linkedHawb){
//                    awb.get().getAwbGoodsDescriptionInfo().addAll(hawb.getAwbGoodsDescriptionInfo());
                    if(hawb.getAwbPackingInfo() != null) {
                        linkedPacks.addAll(hawb.getAwbPackingInfo());
                    }
                }
                awb.get().setAwbPackingInfo(linkedPacks);
                if(awb.get().getAwbGoodsDescriptionInfo() != null && awb.get().getAwbGoodsDescriptionInfo().size() > 0) {
                    calculateGoodsDescription(awb.get().getAwbGoodsDescriptionInfo().get(0), linkedPacks, tenantSettings, new HashMap<>(), true);
                }
            }
            AwbResponse response = convertEntityToDto(awb.get());

            if(request.getIncludeColumns()==null||request.getIncludeColumns().size()==0)
                return ResponseHelper.buildSuccessResponse(response);
            else return ResponseHelper.buildSuccessResponse(partialFetchUtils.fetchPartialListData(response, request.getIncludeColumns()));

        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Transactional
    public ResponseEntity<IRunnerResponse> createMawb(CommonRequestModel commonRequestModel) {
        String responseMsg;

        CreateAwbRequest request = (CreateAwbRequest) commonRequestModel.getData();

        if (request.getConsolidationId() == null) {
            log.error("Consolidation Id can't be null or empty in create MAWB Request");
            throw new ValidationException("Consolidation Id can't be null or empty in Create MAWB Request");
        }

        Awb awb = new Awb();
        try {
            // fetch consolidation info
            ConsolidationDetails consolidationDetails = consolidationDetailsDao.findById(request.getConsolidationId()).get();
            List<Awb> awbList = new ArrayList<>();
            List<AwbPackingInfo> mawbPackingInfo = new ArrayList<>();
            for (var consoleShipment : consolidationDetails.getShipmentsList()) {
                if (consoleShipment.getId() != null) {
                    List<Awb> optionalAwb = awbDao.findByShipmentIdList(Arrays.asList(consoleShipment.getId()));
                    if (optionalAwb == null || optionalAwb.isEmpty()) {
                        throw new ValidationException(AwbConstants.GENERATE_HAWB_BEFORE_MAWB_EXCEPTION);
                    }
                    Awb linkAwb = optionalAwb.stream().findFirst().get();
                    awbList.add(linkAwb);
                    if(linkAwb.getAwbPackingInfo() != null) {
                        mawbPackingInfo.addAll(linkAwb.getAwbPackingInfo());
                    }
                }
            }

            // save awb details
            awb = generateMawb(request, consolidationDetails, mawbPackingInfo);
            if(awbList != null && !awbList.isEmpty())
                updateSciFieldFromMawb(awb, awbList);
            awb = awbDao.save(awb);
            try {
                callV1Sync(awb, SaveStatus.CREATE);
            } catch (Exception e) {
                log.error(SyncingConstants.ERROR_PERFORMING_AWB_SYNC, e);
            }

            // map mawb and hawb affter suuccessful save
            LinkHawbMawb(awb, awbList);
            log.info("MAWB created successfully for Id {} with Request Id {}", awb.getId(), LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(awb));
    }

    public ResponseEntity<IRunnerResponse> updateGoodsAndPacksForMawb(CommonRequestModel commonRequestModel) {
        String responseMsg;
        Awb awb = new Awb();
        try {
            CreateAwbRequest request = (CreateAwbRequest) commonRequestModel.getData();
            if (request == null) {
                log.debug("Request is empty for Update Goods And Packs For Mawb for Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new RunnerException("Request is empty for Update Goods And Packs For Mawb");
            }

            if (request.getShipmentId() == null) {
                log.error("Shipment Id can't be null or empty in Update Goods And Packs For Mawb Request");
                throw new ValidationException("Shipment Id can't be null or empty in Update Goods And Packs For Mawb");
            }

            // awb = awbDao.save(generateAwb(request));
            updateGoodsAndPacks(request); //TODO
            log.info("Update Goods And Packs For Mawb successfully for Id {} with Request Id {}", awb.getId(), LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(awb));
    }

    private void updateGoodsAndPacks(CreateAwbRequest request) throws RunnerException {
        Awb mawb = awbDao.findByConsolidationId(request.getConsolidationId()).get(0);
        List<Awb> linkedHawb = getLinkedAwbFromMawb(mawb.getId());
        List<AwbPackingInfo> allHawbPacks = new ArrayList<>();
        for(var i : linkedHawb) {
            allHawbPacks.addAll(i.getAwbPackingInfo());
        }

        // Get tenant settings
        ShipmentSettingsDetails tenantSettings = commonUtils.getShipmentSettingFromContext();

        if(allHawbPacks.isEmpty() && !Boolean.TRUE.equals(tenantSettings.getConsolidationLite())) {
            updateGoodsDescForMawb(mawb);
        } else if (allHawbPacks.size() > 0) {
            calculateAndUpdateGoodsPacksMawb(allHawbPacks, mawb,tenantSettings);
        }
    }

    private void updateGoodsDescForMawb(Awb mawb) throws RunnerException {
        AwbGoodsDescriptionInfo awbGoodsDescriptionInfo = null;
        if (mawb.getAwbGoodsDescriptionInfo() != null && mawb.getAwbGoodsDescriptionInfo().size() > 0) {
            awbGoodsDescriptionInfo = mawb.getAwbGoodsDescriptionInfo().get(0);
            awbGoodsDescriptionInfo.setPiecesNo(0);
            awbGoodsDescriptionInfo.setGrossWt(BigDecimal.ZERO);
            awbGoodsDescriptionInfo.setChargeableWt(BigDecimal.ZERO);

            mawb.getAwbGoodsDescriptionInfo().set(0, awbGoodsDescriptionInfo);
            awbDao.save(mawb);
        }
    }

    private void calculateAndUpdateGoodsPacksMawb(List<AwbPackingInfo> allHawbPacks, Awb mawb, ShipmentSettingsDetails tenantSettings) throws RunnerException {
        AwbGoodsDescriptionInfo mawbGoodsDescriptionInfo = null;
        if (mawb.getAwbGoodsDescriptionInfo() != null && mawb.getAwbGoodsDescriptionInfo().size() > 0) {
            // V1 always fetches via FirstOrDefault method post repo list call
            mawbGoodsDescriptionInfo = mawb.getAwbGoodsDescriptionInfo().get(0);
            Map<String, List<AwbPackingInfo>> hawbPacksMap = new HashMap<>(); // map to store awbNumber -> packsList

            var pair = calculateGoodsDescription(mawbGoodsDescriptionInfo, allHawbPacks, tenantSettings, hawbPacksMap, true);

            // Can there be a scenario of multiple Goods information ?
            mawb.setAwbGoodsDescriptionInfo(List.of(pair.getRight()));
            saveHawbPacks(mawb, hawbPacksMap);
            awbDao.save(mawb);
        }
    }

    private Pair<BigDecimal, AwbGoodsDescriptionInfo> calculateGoodsDescription(AwbGoodsDescriptionInfo mawbGoodsDescriptionInfo, List<AwbPackingInfo> allHawbPacks, ShipmentSettingsDetails tenantSettings, Map<String, List<AwbPackingInfo>> hawbPacksMap, boolean isPackUpdate) throws RunnerException {
        Long mawbGoodsDescId = null; //mawbGoodsDescriptionInfo.getId();  // TODO goodsDescId where to get this
        UUID mawbGoodsDescGuid = mawbGoodsDescriptionInfo.getGuid();
        Integer noOfPacks = 0;
        BigDecimal totalGrossVolumeOfMawbGood = BigDecimal.ZERO;
        BigDecimal totalGrossWeightOfMawbGood = BigDecimal.ZERO;
        BigDecimal chargeableWeightOfMawbGood = BigDecimal.ZERO;
        BigDecimal totalAmountOfMawbGood = BigDecimal.ZERO;
        String grossWeightUnit = "";

        BigDecimal totalVolumetricWeight = BigDecimal.ZERO;

        if(allHawbPacks != null && allHawbPacks.size() > 0) {
            for (var i : allHawbPacks) {
                noOfPacks += Integer.parseInt(Objects.isNull(i.getPacks()) ? "0" : i.getPacks());
                try {
                    // Populate volume related fields
                    if (i.getVolume() != null) {
                        if (i.getVolumeUnit() == null || i.getVolumeUnit().isEmpty())
                            totalGrossVolumeOfMawbGood = totalGrossVolumeOfMawbGood.add(convertToBigDecimal(unitConversionUtility.convertUnit(Constants.VOLUME, i.getVolume(), Constants.VOLUME_UNIT_M3, tenantSettings.getVolumeChargeableUnit())));

                        else
                            totalGrossVolumeOfMawbGood = totalGrossVolumeOfMawbGood.add(convertToBigDecimal(unitConversionUtility.convertUnit(Constants.VOLUME, i.getVolume(), i.getVolumeUnit(), tenantSettings.getVolumeChargeableUnit())));
                    }
                    // Populate weight related fields
                    if (i.getWeight() != null) {
                        if (i.getWeightUnit() == null || i.getWeightUnit().isEmpty())
                            totalGrossWeightOfMawbGood = totalGrossWeightOfMawbGood.add(convertToBigDecimal(unitConversionUtility.convertUnit(Constants.MASS, i.getWeight(), Constants.WEIGHT_UNIT_KG, tenantSettings.getWeightChargeableUnit())));
                        else
                            totalGrossWeightOfMawbGood = totalGrossWeightOfMawbGood.add(convertToBigDecimal(unitConversionUtility.convertUnit(Constants.MASS, i.getWeight(), i.getWeightUnit(), tenantSettings.getWeightChargeableUnit())));
                    }

                    if (hawbPacksMap.get(i.getAwbNumber()) == null) {
                        hawbPacksMap.put(i.getAwbNumber(), new ArrayList<>());
                    } else {
                        List<AwbPackingInfo> existingPacks = hawbPacksMap.get(i.getAwbNumber());
                        existingPacks.add(i);
                        hawbPacksMap.put(i.getAwbNumber(), existingPacks);
                    }

                } catch (Exception e) {
                    log.error(e.getMessage());
                    throw new RunnerException(e.getMessage());
                }
            }
            if (tenantSettings != null && Constants.VOLUME_UNIT_M3.equalsIgnoreCase(tenantSettings.getVolumeChargeableUnit()) && Constants.WEIGHT_UNIT_KG.equalsIgnoreCase(tenantSettings.getWeightChargeableUnit())) {
                grossWeightUnit = Constants.WEIGHT_UNIT_KG;
                chargeableWeightOfMawbGood = totalGrossWeightOfMawbGood;
                BigDecimal volumetricWeightOfMawbGood = totalGrossVolumeOfMawbGood.multiply(BigDecimal.valueOf(Constants.FACTOR_VOL_WT));
                chargeableWeightOfMawbGood = chargeableWeightOfMawbGood.max(volumetricWeightOfMawbGood);
                totalVolumetricWeight = volumetricWeightOfMawbGood;
            }
        }

        if(isPackUpdate) {
            mawbGoodsDescriptionInfo.setGrossWt(totalGrossWeightOfMawbGood);
            mawbGoodsDescriptionInfo.setGrossWtUnit(grossWeightUnit);
            mawbGoodsDescriptionInfo.setPiecesNo(noOfPacks);
            mawbGoodsDescriptionInfo.setChargeableWt(roundOffAirShipment(chargeableWeightOfMawbGood));
        }

        if (mawbGoodsDescriptionInfo.getRateCharge() != null && mawbGoodsDescriptionInfo.getRateClass() != null) {
            if (mawbGoodsDescriptionInfo.getRateClass() == 1)
                mawbGoodsDescriptionInfo.setTotalAmount(mawbGoodsDescriptionInfo.getRateCharge());
            else
                mawbGoodsDescriptionInfo.setTotalAmount(mawbGoodsDescriptionInfo.getChargeableWt() != null? mawbGoodsDescriptionInfo.getRateCharge().multiply(mawbGoodsDescriptionInfo.getChargeableWt()) : BigDecimal.ZERO);
        }
        totalAmountOfMawbGood = mawbGoodsDescriptionInfo.getTotalAmount();
        mawbGoodsDescriptionInfo.setTotalAmount(totalAmountOfMawbGood);
        return Pair.of(totalVolumetricWeight, mawbGoodsDescriptionInfo);
    }

    private void saveHawbPacks(Awb mawb, Map<String, List<AwbPackingInfo>> hawbPacksMap) {
        List<String> awbNumbers = hawbPacksMap.keySet().stream().toList();
        ListCommonRequest listCommonRequest = CommonUtils.constructListCommonRequest("awbNumber", awbNumbers, "IN");
        Pair<Specification<Awb>, Pageable> pair = fetchData(listCommonRequest, Awb.class);
        Page<Awb> page = awbDao.findAll(pair.getLeft(), pair.getRight());

        if(!page.isEmpty()) {
            List<Awb> hawbList = page.getContent();
            for(var hawb : hawbList) {
                hawb.setAwbPackingInfo(hawbPacksMap.get(hawb.getAwbNumber()));
            }
            awbDao.saveAll(hawbList);
        }
    }

    private static BigDecimal roundOffAirShipment(BigDecimal charge) {
        if (charge.subtract(new BigDecimal("0.50")).compareTo(charge.setScale(0, BigDecimal.ROUND_FLOOR)) <= 0
                && charge.compareTo(charge.setScale(0, BigDecimal.ROUND_FLOOR)) != 0) {
            charge = charge.setScale(0, BigDecimal.ROUND_FLOOR).add(new BigDecimal("0.50"));
        } else {
            charge = charge.setScale(0, BigDecimal.ROUND_CEILING);
        }
        return charge;
    }

    private AwbResponse convertEntityToDto(Awb awbShipmentInfo) {
        var awbResponse = jsonHelper.convertValue(awbShipmentInfo, AwbResponse.class);
        if(awbShipmentInfo.getAwbSpecialHandlingCodesMappings() != null && awbShipmentInfo.getAwbSpecialHandlingCodesMappings().size() > 0) {
            awbResponse.setShcIdList(awbShipmentInfo.getAwbSpecialHandlingCodesMappings().stream()
                    .map(i -> i.getShcId())
                    .toList()
            );
        }
        return awbResponse;
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<Awb> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        List<String> chargeCodes = new ArrayList<>();
        lst.forEach(awbShipmentInfo -> {
            var res = convertEntityToDto(awbShipmentInfo);
            var chargeCode = res.getAwbCargoInfo() != null ? res.getAwbCargoInfo().getChargeCode() : null;
            if(chargeCode != null){
                CommonV1ListRequest request = new CommonV1ListRequest();
                List<Object> criteria = new ArrayList<>();
                List<Object> subCriteria1 = Arrays.asList(
                        Arrays.asList(MasterDataConstants.ITEM_TYPE),
                        "=",
                        "105"
                );
                List<Object> subCriteria2 = Arrays.asList(
                        Arrays.asList(MasterDataConstants.ITEM_VALUE),
                        "=",
                        chargeCode
                );
                criteria.addAll(List.of(subCriteria1, "and", subCriteria2));
                request.setCriteriaRequests(criteria);
                try {
                    V1DataResponse response = v1Service.fetchMasterData(request);
                    List<MasterData> entityTransferMasterLists = jsonHelper.convertValueToList(response.entities, MasterData.class);
                    if(entityTransferMasterLists != null && entityTransferMasterLists.size() > 0)
                        res.setChargeDetails(entityTransferMasterLists.get(0));
                } catch (Exception ignored) {}
            }
            generateDefaultAwbInformation(awbShipmentInfo, res);
            String error = null;
            if(res.getAwbShipmentInfo().getEntityType().equals(Constants.MAWB) || res.getAwbShipmentInfo().getEntityType().equals(Constants.DMAWB)){
                ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
                Boolean fetchRatesWarning = awbShipmentInfo.getAwbGoodsDescriptionInfo().stream().anyMatch(x -> x.getRateCharge() != null && Boolean.TRUE.equals(x.getEnableFetchRatesWarning()));
                if(Boolean.TRUE.equals(shipmentSettingsDetails.getIataTactFlag()) && Boolean.TRUE.equals(fetchRatesWarning)){
                    error = "The Port/ Carrier details are changed - You need to fetch the new TACT Rates.";
                }
            }
            try {
                res.setErrors(validateAwb(awbShipmentInfo));
            } catch (RunnerException e) {
                throw new RuntimeException(e);
            }
            if(error != null)
                res.setErrors(res.getErrors()!=null ? String.join("\r\n", res.getErrors(), error): error);
            responseList.add(res);
        });
        return responseList;
    }

    private Awb convertRequestToEntity(AwbRequest request) {
        var awb =  jsonHelper.convertValue(request, Awb.class);
        if(request.getShcIdList() != null) {
            List<AwbSpecialHandlingCodesMappingInfo> res = new ArrayList<>();
            for(var shcId : request.getShcIdList()) {
                res.add(AwbSpecialHandlingCodesMappingInfo.builder()
                        .entityId(request.getAwbShipmentInfo().getEntityId())
                        .entityType(request.getAwbShipmentInfo().getEntityType())
                        .shcId(shcId)
                        .build());
            }
            awb.setAwbSpecialHandlingCodesMappings(res);
        }
        return awb;
    }

    private Awb generateMawb(CreateAwbRequest request, ConsolidationDetails consolidationDetails, List<AwbPackingInfo> awbPackingInfo) throws RunnerException {

        if(request.getIsReset() == null || request.getIsReset() == false) {
            List<Awb> existingAwbs = awbDao.findByConsolidationId(request.getConsolidationId());
            if(existingAwbs.size() > 0)
                throw new RunnerException("MAWB already created for current Consolidation !");
        }

        // validate the request
        AwbUtility.validateConsolidationInfoBeforeGeneratingAwb(consolidationDetails);

        List<Awb> awbList = new ArrayList<>();
        List<AwbPackingInfo> mawbPackingInfo = new ArrayList<>();
        for (var consoleShipment : consolidationDetails.getShipmentsList()) {
            if (consoleShipment.getId() != null) {
                Optional<Awb> linkAwb = awbDao.findByShipmentIdList(Arrays.asList(consoleShipment.getId())).stream().findFirst();
                if (linkAwb.isEmpty()) {
                    throw new ValidationException(AwbConstants.GENERATE_HAWB_BEFORE_MAWB_EXCEPTION);
                }
                awbList.add(linkAwb.get());
                if(linkAwb.get().getAwbPackingInfo() != null) {
                    mawbPackingInfo.addAll(linkAwb.get().getAwbPackingInfo());
                }
            }
        }

        //var awbPackingInfo = generateMawbPackingInfo(consolidationDetails);
        // generate Awb Entity

        List<AwbSpecialHandlingCodesMappingInfo> sph = new ArrayList<>();
        V1TenantSettingsResponse tenantSettingsResponse = commonUtils.getCurrentTenantSettings();
        if(Boolean.TRUE.equals(tenantSettingsResponse.getEnableAirMessaging()) && !Strings.isNullOrEmpty(consolidationDetails.getEfreightStatus())
                && (consolidationDetails.getEfreightStatus().equalsIgnoreCase(Constants.EAW)
         || consolidationDetails.getEfreightStatus().equalsIgnoreCase(Constants.EAP))) {
            sph.add(AwbSpecialHandlingCodesMappingInfo
                    .builder()
                    .shcId(consolidationDetails.getEfreightStatus())
                    .entityId(consolidationDetails.getId())
                    .entityType(request.getAwbType())
                    .build());
        }
        if(!Strings.isNullOrEmpty(consolidationDetails.getSecurityStatus()) && AwbConstants.SecurityStatusList.contains(consolidationDetails.getSecurityStatus())) {
            sph.add(AwbSpecialHandlingCodesMappingInfo
                    .builder()
                    .shcId(consolidationDetails.getSecurityStatus())
                    .entityId(consolidationDetails.getId())
                    .entityType(request.getAwbType())
                    .build());
        }

        try {
            consolidationService.validateRaKcForConsol(consolidationDetails);
        }catch (ValidationException ex){
            throw new RunnerException(ex.getMessage());
        }catch (Exception e) {
            throw new RunnerException(errorMessage);
        }

        AwbCargoInfo awbCargoInfo = new AwbCargoInfo();
        Awb awb = Awb.builder()
                .awbNumber(consolidationDetails.getMawb())
                .awbShipmentInfo(generateMawbShipmentInfo(consolidationDetails, request, awbCargoInfo))
                .awbNotifyPartyInfo(generateMawbNotifyPartyinfo(consolidationDetails, request))
                .awbRoutingInfo(generateMawbRoutingInfo(consolidationDetails, request))
                .awbGoodsDescriptionInfo(generateMawbGoodsDescriptionInfo(consolidationDetails, request, null))
                .awbOtherInfo(generateMawbOtherInfo(consolidationDetails, request))
                //.awbPackingInfo(awbPackingInfo)
                .consolidationId(consolidationDetails.getId())
                .awbSpecialHandlingCodesMappings(sph)
                .build();
        awb.setAwbCargoInfo(generateMawbCargoInfo(consolidationDetails, request, awbPackingInfo, awbCargoInfo, awb.getAwbGoodsDescriptionInfo()));
        awb.getAwbCargoInfo().setSci(consolidationDetails.getSci());
        return awb;
    }

    private void updateSciFieldFromMawb(Awb awb, List<Awb> awbList) {
        boolean updateSci = false;
        if(!awbList.isEmpty()){
            Optional<Awb> hawb = awbList.stream().filter(x -> Objects.equals(x.getAwbCargoInfo().getSci(), AwbConstants.T1)).findAny();
            if(hawb.isPresent()){
                updateSci = true;
            }
        }
        if(!Objects.equals(awb.getPrintType(), PrintType.ORIGINAL_PRINTED)) {
            if (updateSci) {
                awb.getAwbCargoInfo().setSci(AwbConstants.T1);
            } else {
                awb.getAwbCargoInfo().setSci(null);
            }
        }
    }

    private AwbShipmentInfo generateMawbShipmentInfo(ConsolidationDetails consolidationDetails, CreateAwbRequest request, AwbCargoInfo awbCargoInfo) throws RunnerException {
        AwbShipmentInfo awbShipmentInfo = new AwbShipmentInfo();
        TenantModel tenantModel = null;
        try {
            tenantModel = jsonHelper.convertValue(v1Service.retrieveTenant().getEntity(), TenantModel.class);
        } catch (Exception e) {
            throw new RunnerException("Failed while fetching tenant info from V1");
        }

        awbShipmentInfo.setEntityId(consolidationDetails.getId());
        awbShipmentInfo.setEntityType(request.getAwbType());
        var shipperName = StringUtility.convertToString(consolidationDetails.getSendingAgent() != null && consolidationDetails.getReceivingAgent().getOrgData() != null ? consolidationDetails.getSendingAgent().getOrgData().get(PartiesConstants.FULLNAME) : "");
        awbShipmentInfo.setShipperName(shipperName == null ? shipperName : shipperName.toUpperCase());
        awbShipmentInfo.setAwbNumber(consolidationDetails.getMawb());
        awbShipmentInfo.setFirstCarrier(consolidationDetails.getCarrierDetails().getShippingLine());
        var shipperAddress = AwbUtility.constructAddress(consolidationDetails.getSendingAgent() != null ? consolidationDetails.getSendingAgent().getAddressData() : null);
        awbShipmentInfo.setShipperAddress(shipperAddress == null ? shipperAddress : shipperAddress.toUpperCase());
        awbShipmentInfo.setShipperReferenceNumber(consolidationDetails.getAgentReference());
        var consigneeName = StringUtility.convertToString(consolidationDetails.getReceivingAgent() != null && consolidationDetails.getReceivingAgent().getOrgData() != null? consolidationDetails.getReceivingAgent().getOrgData().get(PartiesConstants.FULLNAME) : "");
        awbShipmentInfo.setConsigneeName(consigneeName == null ? consigneeName : consigneeName.toUpperCase());
        var consigneeAddress = AwbUtility.constructAddress(consolidationDetails.getReceivingAgent() != null ? consolidationDetails.getReceivingAgent().getAddressData() : null);
        awbShipmentInfo.setConsigneeAddress(consigneeAddress == null ? consigneeAddress : consigneeAddress.toUpperCase());
        awbShipmentInfo.setConsigneeReferenceNumber(consolidationDetails.getReceivingAgent() != null ? consolidationDetails.getReceivingAgent() .getId().toString() : null);
        // AwbUtility.getConsolidationForwarderDetails(uow, consolidationRow, awbShipmentInfo, awbOtherInfoRow, awbCargoInfo); TODO
//        awbShipmentInfo.setOriginAirport(consolidationDetails.getCarrierDetails() != null ? consolidationDetails.getCarrierDetails().getOriginPort() : null);
//        awbShipmentInfo.setDestinationAirport(consolidationDetails.getCarrierDetails() != null ? consolidationDetails.getCarrierDetails().getDestinationPort() : null);
        setAwbShipmentInfoUnLocationData(awbShipmentInfo, consolidationDetails.getCarrierDetails(), false, false);
        setTenantFieldsInAwbShipmentInfo(awbShipmentInfo, tenantModel);

         for (var orgRow : consolidationDetails.getConsolidationAddresses()) {
            if (orgRow.getType().equals(Constants.FAG)) {
                var issuingAgentName = StringUtility.convertToString(orgRow.getOrgData().get(PartiesConstants.FULLNAME));
                awbShipmentInfo.setIssuingAgentName(issuingAgentName == null ? issuingAgentName : issuingAgentName.toUpperCase()); // extract from orgdata
                var issuingAgentAddress = AwbUtility.constructAddress(orgRow.getAddressData());
                awbShipmentInfo.setIssuingAgentAddress(issuingAgentAddress == null ? issuingAgentAddress : issuingAgentAddress.toUpperCase());
                String country = orgRow.getOrgData() != null ?
                        (String) orgRow.getOrgData().get(COUNTRY) : null;
                if (country != null)
                    awbCargoInfo.setCustomOriginCode(getCountryCode(country));

                awbShipmentInfo.setIataCode(StringUtility.isEmpty(awbShipmentInfo.getIataCode())
                        ? StringUtility.convertToString(orgRow.getOrgData().get(PartiesConstants.AGENT_IATA_CODE))
                        : awbShipmentInfo.getIataCode());
                awbShipmentInfo.setAgentCASSCode(StringUtility.isEmpty(awbShipmentInfo.getAgentCASSCode())
                        ? StringUtility.convertToString(orgRow.getOrgData().get(PartiesConstants.AGENT_IATA_CODE))
                        : awbShipmentInfo.getAgentCASSCode());

                String city = orgRow.getOrgData() != null ? stringValueOf(orgRow.getOrgData().get(CITY)) : null;
                if(StringUtility.isNotEmpty(city)) {
                    executedAt = setUnLocationDataWithDiarcties(city);
                } else {
                    executedAt = null;
                }
            }
        }

    if (awbShipmentInfo.getIssuingAgentName() == null || awbShipmentInfo.getIssuingAgentName().isEmpty()) {
        populateIssuingAgent(awbShipmentInfo, tenantModel, awbCargoInfo);
    }

        return awbShipmentInfo;
    }

    private List<AwbNotifyPartyInfo> generateMawbNotifyPartyinfo(ConsolidationDetails consolidationDetails, CreateAwbRequest request) {
        if (consolidationDetails.getConsolidationAddresses() != null &&
                consolidationDetails.getConsolidationAddresses().size() > 0) {
            List<AwbNotifyPartyInfo> notifyPartyList = new ArrayList<>();
            for (var party : consolidationDetails.getConsolidationAddresses()) {
                if (party.getType() != null && (party.getType().equals("Notify Part 1") ||
                        party.getType().equals("Notify Part 2") ||
                        party.getType().equals("Notify Part 3"))) {
                    AwbNotifyPartyInfo notifyPartyInfo = new AwbNotifyPartyInfo();
                    var name = StringUtility.convertToString(party.getOrgData().get(PartiesConstants.FULLNAME));
                    notifyPartyInfo.setName(name == null ? name : name.toUpperCase());
                    notifyPartyInfo.setAddress(AwbUtility.constructAddress(party.getAddressData()).toUpperCase());
                    notifyPartyInfo.setEntityId(consolidationDetails.getId());
                    notifyPartyInfo.setEntityType(request.getAwbType());
                    notifyPartyInfo.setGuid(party.getGuid());
                    notifyPartyInfo.setIsShipmentCreated(true);
                    // org and address data
                    var orgId = party.getOrgData() != null ?  (Integer) party.getOrgData().get("Id") : null;
                    var addressId = party.getAddressData() != null ?  (Integer) party.getAddressData().get("Id") : null;
                    notifyPartyInfo.setOrgId(orgId);
                    notifyPartyInfo.setAddressId(addressId);
                    notifyPartyInfo.setNotifyOrgId(Long.valueOf(orgId));

                    notifyPartyList.add(notifyPartyInfo);
                    
                }
            }

            return notifyPartyList;
        }
        return null;
    }

    private List<AwbRoutingInfo> generateMawbRoutingInfo(ConsolidationDetails consolidationDetails, CreateAwbRequest request) {
        if (consolidationDetails.getRoutingsList() != null && consolidationDetails.getRoutingsList().size() > 0) {
            var sortedRoutingList = consolidationDetails.getRoutingsList().stream().filter(route -> Objects.equals(route.getMode(), Constants.TRANSPORT_MODE_AIR)).collect(Collectors.toList());
            if(sortedRoutingList != null && !sortedRoutingList.isEmpty()) {
                List<AwbRoutingInfo> res = new ArrayList<>();
                Collections.sort(sortedRoutingList, Comparator.comparing(Routings::getLeg));
                Long leg = 1L;
                for (var route : sortedRoutingList) {
                    AwbRoutingInfo awbRoutingInfo = new AwbRoutingInfo();
                    awbRoutingInfo.setIsShipmentCreated(true);
                    awbRoutingInfo.setOriginPortName(route.getPol());
                    awbRoutingInfo.setDestinationPortName(route.getPod());
                    awbRoutingInfo.setByCarrier(route.getCarrier());
                    awbRoutingInfo.setFlightNumber(route.getFlightNumber());
                    awbRoutingInfo.setFlightDate(route.getEtd());
                    awbRoutingInfo.setEntityId(consolidationDetails.getId());
                    awbRoutingInfo.setEntityType(request.getAwbType());
                    awbRoutingInfo.setLeg(leg++);
                    res.add(awbRoutingInfo);
                }
                return res;
            }
        }
        else if (consolidationDetails.getCarrierDetails() != null &&
                consolidationDetails.getCarrierDetails().getOriginPort() != null &&
                consolidationDetails.getCarrierDetails().getDestinationPort() != null
        ) {
            AwbRoutingInfo routingInfo = new AwbRoutingInfo();
            routingInfo.setIsShipmentCreated(true);
            routingInfo.setFlightDate(consolidationDetails.getCarrierDetails().getEtd());
            routingInfo.setOrigin(consolidationDetails.getCarrierDetails().getOriginPort());
            routingInfo.setDestination(consolidationDetails.getCarrierDetails().getDestinationPort());
            routingInfo.setOriginPortName(consolidationDetails.getCarrierDetails().getOriginPort());
            routingInfo.setDestinationPortName(consolidationDetails.getCarrierDetails().getDestinationPort());
            routingInfo.setByCarrier(consolidationDetails.getCarrierDetails().getShippingLine());
            routingInfo.setFlightNumber(consolidationDetails.getCarrierDetails().getFlightNumber());
            routingInfo.setEntityId(consolidationDetails.getId());
            routingInfo.setEntityType(request.getAwbType());
            routingInfo.setLeg(1L);
            return Arrays.asList(routingInfo);
        }
        return null;
    }

    private AwbCargoInfo generateMawbCargoInfo(ConsolidationDetails consolidationDetails, CreateAwbRequest request, List<AwbPackingInfo> awbPackingList, AwbCargoInfo awbCargoInfo, List<AwbGoodsDescriptionInfo> awbGoodsDescriptionInfos) {
        if(awbCargoInfo == null) {
            awbCargoInfo = new AwbCargoInfo();
        }

        List<String> shipmentDescriptions = new ArrayList<>();
        for (ShipmentDetails consoleShipment : consolidationDetails.getShipmentsList()) {
            if (!StringUtility.isEmpty(consoleShipment.getGoodsDescription())) {
                shipmentDescriptions.add(consoleShipment.getGoodsDescription());
            }
        }
        String concatenatedGoodsDesc = "";
        if (shipmentDescriptions.size() > 0) {
            concatenatedGoodsDesc = String.join(",", shipmentDescriptions);
        }

        String defaultTextForQuantAndGoods = Constants.DEFAULT_NATURE_AND_QUANTITY_GOODS_TEXT_MAWB;
        String newLine = "\r\n";

        awbCargoInfo.setNtrQtyGoods(concatenatedGoodsDesc);
        GenerateAwbPaymentInfoRequest generateAwbPaymentInfoRequest = new GenerateAwbPaymentInfoRequest();
        generateAwbPaymentInfoRequest.setAwbCargoInfo(awbCargoInfo);
        generateAwbPaymentInfoRequest.setAwbPackingInfo(awbPackingList);
        generateAwbPaymentInfoRequest.setAwbGoodsDescriptionInfo(awbGoodsDescriptionInfos);
        generateAwbPaymentInfoRequest.setIsFromShipment(false);
        generateAwbPaymentInfoRequest.setPackUpdate(false);
        awbCargoInfo.setNtrQtyGoods(defaultTextForQuantAndGoods + newLine + getDims(generateAwbPaymentInfoRequest));
        awbCargoInfo.setEntityId(consolidationDetails.getId());
        awbCargoInfo.setEntityType(request.getAwbType());
//        awbCargoInfo.setCarriageValue(shipmentDetails.getGoodsValue() != null ? shipmentDetails.getGoodsValue() : new BigDecimal(0.0)); // field missing
//        awbCargoInfo.setCarriageValue(shipmentDetails.getInsuranceValue() != null ? shipmentDetailsgetInsuranceValue() : new BigDecimal(0.0)); // field missing
        awbCargoInfo.setCustomsValue(new BigDecimal(0.0));
        awbCargoInfo.setCurrency(userContext.getUser().getCompanyCurrency());
        awbCargoInfo.setHandlingInfo(getHandlingInfo(MasterDataType.MAWB_GENERATION, awbPackingList, consolidationDetails.getHazardous()));
        awbCargoInfo.setAccountingInfo(awbCargoInfo.getAccountingInfo() == null ? null : awbCargoInfo.getAccountingInfo().toUpperCase());
        awbCargoInfo.setOtherInfo(awbCargoInfo.getOtherInfo() == null ? null : awbCargoInfo.getOtherInfo().toUpperCase());
        awbCargoInfo.setNtrQtyGoods(awbCargoInfo.getNtrQtyGoods() == null ? null : awbCargoInfo.getNtrQtyGoods().toUpperCase());
        awbCargoInfo.setShippingInformation(awbCargoInfo.getShippingInformation() == null ? null : awbCargoInfo.getShippingInformation().toUpperCase());
        awbCargoInfo.setShippingInformationOther(awbCargoInfo.getShippingInformationOther() == null ? null : awbCargoInfo.getShippingInformationOther().toUpperCase());
//        var consolOptional = consolidationDetailsDao.findById(request.getConsolidationId());
//        if (consolOptional.isPresent()) {
//            var consol = consolOptional.get();
//            for (var consolAddressRow : consol.getConsolidationAddresses()) {
//                if (consolAddressRow.getType() != null && consolAddressRow.getType().equals(Constants.FORWARDING_AGENT)) {
//                    String country = consolAddressRow.getAddressData() != null ?
//                            (String) consolAddressRow.getAddressData().get(COUNTRY) : null;
//                    if (country != null)
//                        awbCargoInfo.setCustomOriginCode(getCountryCode(country));
//                }
//            }
//        }
        awbCargoInfo.setChargeCode(fetchChargeCodes(consolidationDetails.getPayment()));
        return awbCargoInfo;
    }

    private List<AwbGoodsDescriptionInfo> generateMawbGoodsDescriptionInfo(ConsolidationDetails consolidationDetails, CreateAwbRequest request, List<AwbPackingInfo> awbPackingList) {
        AwbGoodsDescriptionInfo awbGoodsDescriptionInfo = new AwbGoodsDescriptionInfo();
        awbGoodsDescriptionInfo.setEntityId(consolidationDetails.getId());
        awbGoodsDescriptionInfo.setEntityType(request.getAwbType());
        awbGoodsDescriptionInfo.setGrossWtUnit("KG");
        awbGoodsDescriptionInfo.setIsShipmentCreated(true);
        awbGoodsDescriptionInfo.setGuid(UUID.randomUUID());
        if(awbPackingList != null) {
            for (var awbPacking : awbPackingList) {
                awbPacking.setAwbGoodsDescriptionInfoGuid(awbGoodsDescriptionInfo.getGuid());
            }
            awbGoodsDescriptionInfo.setAwbPackingInfo(awbPackingList);
        }
        return Arrays.asList(awbGoodsDescriptionInfo);
    }

    private AwbOtherInfo generateMawbOtherInfo(ConsolidationDetails consolidationDetails, CreateAwbRequest request) {
        AwbOtherInfo awbOtherInfo = new AwbOtherInfo();
        awbOtherInfo.setEntityId(consolidationDetails.getId());
        awbOtherInfo.setEntityType(request.getAwbType());
        var shipperName = StringUtility.convertToString(consolidationDetails.getSendingAgent() != null && consolidationDetails.getReceivingAgent().getOrgData() != null ? consolidationDetails.getSendingAgent().getOrgData().get(PartiesConstants.FULLNAME) : "");
        awbOtherInfo.setShipper(shipperName == null ? null : shipperName.toUpperCase());
        awbOtherInfo.setExecutedOn(jsonHelper.convertValue(DateTimeFormatter.ofPattern(Constants.YYYY_MM_DD_T_HH_MM_SS).format(LocalDateTime.now()), LocalDateTime.class));
        awbOtherInfo.setExecutedAt(executedAt);
        getAwbOtherInfoMasterData(awbOtherInfo, request.getAwbType());
        return awbOtherInfo;
    }

    private void LinkHawbMawb(Awb mawb, List<Awb> awbList) throws RunnerException {
        for (var awb : awbList) {
            if(awb.getAwbPackingInfo() != null) {
                for(AwbPackingInfo awbPackingInfo : awb.getAwbPackingInfo()) {
                    awbPackingInfo.setMawbGoodsDescGuid(mawb.getAwbGoodsDescriptionInfo() == null || mawb.getAwbGoodsDescriptionInfo().size() == 0 ? null : mawb.getAwbGoodsDescriptionInfo().get(0).getGuid());
                }
            }
            awbDao.save(awb);

            MawbHawbLink mawbHawblink = new MawbHawbLink();
            mawbHawblink.setHawbId(awb.getId());
            mawbHawblink.setMawbId(mawb.getId());
            mawbHawbLinkDao.save(mawbHawblink);
        }
    }

//    private List<AwbPackingInfo> generateMawbPackingInfo(ConsolidationDetails consolidationDetails) {
//        List<AwbPackingInfo> awbPackingList = new ArrayList<>();
//        List<AwbPackingInfo> hawbPacksLinkedToMawb = new ArrayList<>();
//
//        if (consolidationDetails.getShipmentsList().size() > 0) {
//            for (var consoleShipment : consolidationDetails.getShipmentsList()) {
//                if (!StringUtility.isEmpty(consoleShipment.getGoodsDescription())) {
//                    attachedShipmentDescriptions.add(consoleShipment.getGoodsDescription());
//                }
//
//                var awbList = awbDao.findByShipmentId(consoleShipment.getId());
//                if (awbList == null || awbList.size() == 0) {
//                    throw new ValidationException(AwbConstants.GENERATE_HAWB_BEFORE_MAWB_EXCEPTION);
//                }
//
//                var awb = awbList.stream().findFirst().get();
//                if (awb.getAwbPackingInfo() != null && awb.getAwbPackingInfo().size() > 0) {
//                    for (var awbPack : awb.getAwbPackingInfo()) {
//                        if (awbPack.getVolume() != null && !StringUtility.isEmpty(awbPack.getVolumeUnit()) &&
//                                awbPack.getVolumeUnit() == "M3") {
//                            totalVolumetricWeightOfAwbPacks.add(awbPack.getVolume());
//                        }
//                        hawbPacksLinkedToMawb.add(awbPack);
//                    }
//                }
//            }
//            Double factor = Constants.FACTOR_VOL_WT;
//            totalVolumetricWeightOfAwbPacks.multiply(new BigDecimal(factor));
//        }
//        return hawbPacksLinkedToMawb;
//    }

    private Awb generateAwb(CreateAwbRequest request) throws RunnerException {

        if(request.getIsReset() == null || request.getIsReset() == false) {
            List<Awb> existingAwbs = awbDao.findByShipmentId(request.getShipmentId());
            if(existingAwbs.size() > 0)
                throw new RunnerException("AWB already created for current Shipment !");
        }

        // fetch sehipment info
        ShipmentDetails shipmentDetails = shipmentDao.findById(request.getShipmentId()).get();
        boolean syncShipment = false;
        if(StringUtility.isEmpty(shipmentDetails.getHouseBill())) {
            if(!(Objects.equals(Constants.SHIPMENT_TYPE_DRT, shipmentDetails.getJobType()) && Objects.equals(Constants.TRANSPORT_MODE_AIR, shipmentDetails.getTransportMode())))
            shipmentDetails.setHouseBill(shipmentService.generateCustomHouseBL(shipmentDetails));
            shipmentDao.save(shipmentDetails, false);
            syncShipment = true;
        }

        // fetch all packings
        List<Packing> packings = shipmentDetails.getPackingList();

        // Generate HAWB Number if restrictHBlGeneration && numberSequencing
        // shipmentDetails.setHouseBill(generateCustomizedBLNumber(shipmentDetails)); //TODO - implement logic to generate house bill

        // validate the request
        AwbUtility.validateShipmentInfoBeforeGeneratingAwb(shipmentDetails);

        var awbPackingInfo = generateAwbPackingInfo(shipmentDetails, packings);
        if(syncShipment) {
            try {
                shipmentSync.sync(shipmentDetails, null, null, UUID.randomUUID().toString(), false);
            } catch (Exception e) {
                log.error("Error performing sync on shipment entity, {}", e);
            }
        }
        List<AwbSpecialHandlingCodesMappingInfo> sph = new ArrayList<>();
        V1TenantSettingsResponse tenantSettingsResponse = commonUtils.getCurrentTenantSettings();
        if(Boolean.TRUE.equals(tenantSettingsResponse.getEnableAirMessaging()) && !Strings.isNullOrEmpty(shipmentDetails.getAdditionalDetails().getEfreightStatus())
            && (shipmentDetails.getAdditionalDetails().getEfreightStatus().equalsIgnoreCase(Constants.EAW) ||
                (Objects.equals(shipmentDetails.getJobType(), Constants.SHIPMENT_TYPE_DRT) && shipmentDetails.getAdditionalDetails().getEfreightStatus().equalsIgnoreCase(Constants.EAP)))) {
            sph.add(AwbSpecialHandlingCodesMappingInfo
                    .builder()
                    .shcId(shipmentDetails.getAdditionalDetails().getEfreightStatus())
                    .entityId(shipmentDetails.getId())
                    .entityType(request.getAwbType())
                    .build());
        }
        if(!Strings.isNullOrEmpty(shipmentDetails.getSecurityStatus()) && AwbConstants.SecurityStatusList.contains(shipmentDetails.getSecurityStatus())) {
            sph.add(AwbSpecialHandlingCodesMappingInfo
                    .builder()
                    .shcId(shipmentDetails.getSecurityStatus())
                    .entityId(shipmentDetails.getId())
                    .entityType(request.getAwbType())
                    .build());
        }

        try{
            shipmentService.validateRaKcDetails(shipmentDetails);
        }catch (ValidationException ex){
            throw new RunnerException(ex.getMessage());
        }catch (Exception ex){
            throw new RunnerException(errorMessage);
        }

        AwbCargoInfo awbCargoInfo = new AwbCargoInfo();
        // generate Awb Entity
        Awb awb = Awb.builder()
                .awbNumber(request.getAwbType().equals(Constants.DMAWB) ? shipmentDetails.getMasterBill() : shipmentDetails.getHouseBill())
                .awbShipmentInfo(generateAwbShipmentInfo(shipmentDetails, request, awbCargoInfo))
                .awbNotifyPartyInfo(generateAwbNotifyPartyinfo(shipmentDetails, request))
                .awbRoutingInfo(generateAwbRoutingInfo(shipmentDetails, request))
                .awbGoodsDescriptionInfo(generateAwbGoodsDescriptionInfo(shipmentDetails, request, awbPackingInfo))
                .awbOtherInfo(generateAwbOtherInfo(shipmentDetails, request))
                .awbPackingInfo(awbPackingInfo)
                .shipmentId(shipmentDetails.getId())
                .awbSpecialHandlingCodesMappings(sph)
                .build();
        awb.setAwbCargoInfo(generateAwbCargoInfo(shipmentDetails, request, awbPackingInfo, awbCargoInfo, awb.getAwbGoodsDescriptionInfo()));
        awb.getAwbCargoInfo().setSci(shipmentDetails.getAdditionalDetails().getSci());
        return awb;
    }

    private AwbShipmentInfo generateAwbShipmentInfo(ShipmentDetails shipmentDetails, CreateAwbRequest request, AwbCargoInfo awbCargoInfo) throws RunnerException {
        AwbShipmentInfo awbShipmentInfo = new AwbShipmentInfo();
        awbShipmentInfo.setEntityId(shipmentDetails.getId());
        awbShipmentInfo.setEntityType(request.getAwbType());
        awbShipmentInfo.setAwbNumber(request.getAwbType().equals(Constants.DMAWB) ? shipmentDetails.getMasterBill() : shipmentDetails.getHouseBill());
        var shipperName = StringUtility.convertToString(shipmentDetails.getConsigner() != null && shipmentDetails.getConsigner().getOrgData() != null ? shipmentDetails.getConsigner().getOrgData().get(PartiesConstants.FULLNAME): "");
        awbShipmentInfo.setShipperName(shipperName == null ? shipperName : shipperName.toUpperCase());
        var shipperAddress = AwbUtility.constructAddress(shipmentDetails.getConsigner() != null ? shipmentDetails.getConsigner().getAddressData() : null);
        awbShipmentInfo.setShipperAddress(shipperAddress == null ? shipperAddress : shipperAddress.toUpperCase());
        var consigneeName = StringUtility.convertToString(shipmentDetails.getConsignee() != null && shipmentDetails.getConsignee().getOrgData() != null? shipmentDetails.getConsignee().getOrgData().get(PartiesConstants.FULLNAME) : "");
        awbShipmentInfo.setConsigneeName(consigneeName == null ? consigneeName : consigneeName.toUpperCase());
        var consigneeAddress = AwbUtility.constructAddress(shipmentDetails.getConsignee() != null ? shipmentDetails.getConsignee().getAddressData() : null);
        awbShipmentInfo.setConsigneeAddress(consigneeAddress == null ? consigneeAddress : consigneeAddress.toUpperCase());

        awbShipmentInfo.setConsigneeReferenceNumber(shipmentDetails.getConsignee() != null ? shipmentDetails.getConsignee().getId().toString() : null);
        setAwbShipmentInfoUnLocationData(awbShipmentInfo,shipmentDetails.getCarrierDetails(), false, false);

        awbShipmentInfo.setFirstCarrier(shipmentDetails.getCarrierDetails() != null ? shipmentDetails.getCarrierDetails().getShippingLine() : null);

        for (var orgRow : shipmentDetails.getShipmentAddresses()) {
            if (orgRow.getType().equals(Constants.FAG)) {
                var issuingAgentName = StringUtility.convertToString(orgRow.getOrgData().get(PartiesConstants.FULLNAME));
                awbShipmentInfo.setIssuingAgentName(issuingAgentName == null ? issuingAgentName : issuingAgentName.toUpperCase()); // extract from orgdata
                var issuingAgentAddress = AwbUtility.constructAddress(orgRow.getAddressData());
                awbShipmentInfo.setIssuingAgentAddress(issuingAgentAddress == null ? issuingAgentAddress : issuingAgentAddress.toUpperCase());
                String country = orgRow.getOrgData() != null ?
                        (String) orgRow.getOrgData().get(COUNTRY) : null;
                if (country != null)
                    awbCargoInfo.setCustomOriginCode(getCountryCode(country));

                awbShipmentInfo.setIataCode(StringUtility.isEmpty(awbShipmentInfo.getIataCode())
                        ? StringUtility.convertToString(shipmentDetails.getConsignee().getOrgData().get(PartiesConstants.AGENT_IATA_CODE))
                        : awbShipmentInfo.getIataCode());
                awbShipmentInfo.setAgentCASSCode(StringUtility.isEmpty(awbShipmentInfo.getAgentCASSCode())
                        ? StringUtility.convertToString(shipmentDetails.getConsignee().getOrgData().get(PartiesConstants.AGENT_CASS_CODE))
                        : awbShipmentInfo.getAgentCASSCode());
                // awbOtherInfoRow.setExecutedAt(getCityId(orgRow.OrgId)); // fetch from master data
                // awbCargoInfo.CustomOriginCode(getCountryCode(orgRow.OrgCountry)); // fetch from master data
                String city = orgRow.getOrgData() != null ? stringValueOf(orgRow.getOrgData().get(CITY)) : null;
                if(StringUtility.isNotEmpty(city)) {
                    executedAt = setUnLocationDataWithDiarcties(city);
                } else {
                    executedAt = null;
                }
            }
        }

        try {
            TenantModel tenantModel = jsonHelper.convertValue(v1Service.retrieveTenant().getEntity(), TenantModel.class);
            setTenantFieldsInAwbShipmentInfo(awbShipmentInfo, tenantModel);
            if (awbShipmentInfo.getIssuingAgentName() == null || awbShipmentInfo.getIssuingAgentName().isEmpty()) {
                populateIssuingAgent(awbShipmentInfo, tenantModel, awbCargoInfo);
            }
        } catch (Exception e) {
            throw new RunnerException(String.format("Error while populating tenant fields in AwbShipmentInfo %s", e.getMessage()));
        }

        return awbShipmentInfo;
    }

    private List<AwbNotifyPartyInfo> generateAwbNotifyPartyinfo(ShipmentDetails shipmentDetails, CreateAwbRequest request) {
        if (shipmentDetails.getAdditionalDetails() != null &&
                shipmentDetails.getAdditionalDetails().getNotifyParty() != null &&
                shipmentDetails.getAdditionalDetails().getNotifyParty().getId() != null) {
            var shipmentNotifyParty = shipmentDetails.getAdditionalDetails().getNotifyParty();
            AwbNotifyPartyInfo notifyPartyInfo = new AwbNotifyPartyInfo();
            notifyPartyInfo.setIsShipmentCreated(true);
            var name = StringUtility.convertToString(shipmentNotifyParty.getOrgData().get(PartiesConstants.FULLNAME));
            notifyPartyInfo.setName(name == null ? name : name.toUpperCase());
            notifyPartyInfo.setAddress(AwbUtility.constructAddress(shipmentNotifyParty.getAddressData()).toUpperCase());
            notifyPartyInfo.setEntityId(shipmentDetails.getId());
            notifyPartyInfo.setEntityType(request.getAwbType());
            // notifyPartyInfo.setAddressId(shipmentNotifyParty.getAddressData()); // field missing: AddressId
            notifyPartyInfo.setNotifyOrgId(shipmentNotifyParty.getId());
            return Arrays.asList(notifyPartyInfo);
        }
        return null;
    }

    private List<AwbRoutingInfo> generateAwbRoutingInfo(ShipmentDetails shipmentDetails, CreateAwbRequest request) {
        if (shipmentDetails.getRoutingsList() != null && shipmentDetails.getRoutingsList().size() > 0) {
            var sortedRoutingList = shipmentDetails.getRoutingsList().stream().filter(route -> Objects.equals(route.getMode(), Constants.TRANSPORT_MODE_AIR)).collect(Collectors.toList());
            if(sortedRoutingList != null && !sortedRoutingList.isEmpty()) {
                List<AwbRoutingInfo> res = new ArrayList<>();
                Collections.sort(sortedRoutingList, Comparator.comparing(Routings::getLeg));
                Long leg = 1L;
                for (var route : sortedRoutingList) {
                    AwbRoutingInfo awbRoutingInfo = new AwbRoutingInfo();
                    awbRoutingInfo.setIsShipmentCreated(true);
                    awbRoutingInfo.setOriginPortName(route.getPol());
                    awbRoutingInfo.setDestinationPortName(route.getPod());
                    awbRoutingInfo.setByCarrier(route.getCarrier());
                    awbRoutingInfo.setFlightNumber(route.getFlightNumber());
                    awbRoutingInfo.setFlightDate(route.getEtd());
                    awbRoutingInfo.setEntityId(shipmentDetails.getId());
                    awbRoutingInfo.setEntityType(request.getAwbType());
                    awbRoutingInfo.setLeg(leg++);
                    res.add(awbRoutingInfo);
                }
                return res;
            }
        }

        else if (shipmentDetails.getCarrierDetails() != null &&
                shipmentDetails.getCarrierDetails().getOriginPort() != null &&
                shipmentDetails.getCarrierDetails().getDestinationPort() != null
        ) {
            var flightDate = shipmentDetails.getCarrierDetails().getEtd();
            AwbRoutingInfo routingInfo = new AwbRoutingInfo();
            routingInfo.setIsShipmentCreated(true);
            routingInfo.setLeg(1L);
//            routingInfo.setOrigin(shipmentDetails.getCarrierDetails().getOriginPort()); // field missing: POLId
//            routingInfo.setDestination(shipmentDetails.getCarrierDetails().getDestinationPort()); // field missing PODId:
            routingInfo.setOriginPortName(shipmentDetails.getCarrierDetails().getOriginPort());
            routingInfo.setDestinationPortName(shipmentDetails.getCarrierDetails().getDestinationPort());
            routingInfo.setByCarrier(shipmentDetails.getCarrierDetails().getShippingLine());
            routingInfo.setFlightNumber(shipmentDetails.getCarrierDetails().getFlightNumber());
            routingInfo.setFlightDate(flightDate);
            routingInfo.setEntityId(shipmentDetails.getId());
            routingInfo.setEntityType(request.getAwbType());
            return Arrays.asList(routingInfo);
        }
        return null;
    }

    private AwbCargoInfo generateAwbCargoInfo(ShipmentDetails shipmentDetails, CreateAwbRequest request, List<AwbPackingInfo> awbPackingList, AwbCargoInfo awbCargoInfo, List<AwbGoodsDescriptionInfo> awbGoodsDescriptionInfos) {
        if(awbCargoInfo == null) {
            awbCargoInfo = new AwbCargoInfo();
        }
        awbCargoInfo.setNtrQtyGoods(shipmentDetails.getGoodsDescription());
        GenerateAwbPaymentInfoRequest generateAwbPaymentInfoRequest = new GenerateAwbPaymentInfoRequest();
        generateAwbPaymentInfoRequest.setAwbCargoInfo(awbCargoInfo);
        generateAwbPaymentInfoRequest.setAwbPackingInfo(awbPackingList);
        generateAwbPaymentInfoRequest.setAwbGoodsDescriptionInfo(awbGoodsDescriptionInfos);
        generateAwbPaymentInfoRequest.setIsFromShipment(true);
        generateAwbPaymentInfoRequest.setPackUpdate(false);
        awbCargoInfo.setNtrQtyGoods(getDims(generateAwbPaymentInfoRequest));
        awbCargoInfo.setEntityId(shipmentDetails.getId());
        awbCargoInfo.setEntityType(request.getAwbType());
//        awbCargoInfo.setCarriageValue(shipmentDetails.getGoodsValue() != null ? shipmentDetails.getGoodsValue() : new BigDecimal(0.0)); // field missing
//        awbCargoInfo.setCarriageValue(shipmentDetails.getInsuranceValue() != null ? shipmentDetailsgetInsuranceValue() : new BigDecimal(0.0)); // field missing
        awbCargoInfo.setCustomsValue(new BigDecimal(0.0));
        awbCargoInfo.setCurrency(userContext.getUser().getCompanyCurrency());
        awbCargoInfo.setHandlingInfo(getHandlingInfo(MasterDataType.HAWB_GENERATION, awbPackingList, shipmentDetails.getContainsHazardous()));
        awbCargoInfo.setAccountingInfo(awbCargoInfo.getAccountingInfo() == null ? null : awbCargoInfo.getAccountingInfo().toUpperCase());
        awbCargoInfo.setOtherInfo(awbCargoInfo.getOtherInfo() == null ? null : awbCargoInfo.getOtherInfo().toUpperCase());
        awbCargoInfo.setNtrQtyGoods(awbCargoInfo.getNtrQtyGoods() == null ? null : awbCargoInfo.getNtrQtyGoods().toUpperCase());
        awbCargoInfo.setShippingInformation(StringUtility.isEmpty(shipmentDetails.getOrderManagementNumber()) ? null : String.format(AwbConstants.ORDER_NUMBER, shipmentDetails.getOrderManagementNumber()));
        awbCargoInfo.setShippingInformationOther(awbCargoInfo.getShippingInformationOther() == null ? null : awbCargoInfo.getShippingInformationOther().toUpperCase());
        if(request.getAwbType().equalsIgnoreCase("DMAWB"))
            awbCargoInfo.setChargeCode(fetchChargeCodes(shipmentDetails.getPaymentTerms()));

        String csdInfo = populateCsdInfo(shipmentDetails);
        awbCargoInfo.setCsdInfo(csdInfo);

        return awbCargoInfo;
    }

    private String populateCsdInfo(ShipmentDetails shipment) {
        Parties originAgent = shipment.getAdditionalDetails().getExportBroker() != null ? shipment.getAdditionalDetails().getExportBroker() : null;
        OrgAddressResponse orgAddressResponse = v1ServiceUtil.fetchOrgInfoFromV1(Arrays.asList(originAgent));

        Map<String, Map<String, Object>> addressMap = orgAddressResponse.getAddresses();
        Map<String, Object> addressSendingAgent = null;
        String raNumber = "";
        String securityStatus = "";
        String screeningStatus = "";
        String csdInfo = "";

        if(addressMap != null && originAgent != null && addressMap.get(originAgent.getOrgCode() + "#" + originAgent.getAddressCode()) != null) {
            addressSendingAgent = addressMap.get(originAgent.getOrgCode() + "#" + originAgent.getAddressCode());

            // Agent not expired
            if(StringUtility.isNotEmpty(StringUtility.convertToString(addressSendingAgent.get(KCRA_EXPIRY)))) {
                LocalDateTime agentExpiry = LocalDateTime.parse(StringUtility.convertToString(addressSendingAgent.get(KCRA_EXPIRY)));

                if(LocalDateTime.now().isAfter(agentExpiry))
                    return csdInfo;

                if(Boolean.TRUE.equals(addressSendingAgent.get(REGULATED_AGENT))) {
                    raNumber = StringUtility.convertToString(addressSendingAgent.get(KCRA_NUMBER));
                }

                if(shipment.getSecurityStatus() != null) {
                    securityStatus = shipment.getSecurityStatus();
                    // Set security status as SPX whenever we get Exemption Cargo
                    if(AwbConstants.EXEMPTION_CARGO_SECURITY_STATUS.equalsIgnoreCase(securityStatus))
                        securityStatus = AwbConstants.SPX + String.format("/%s", shipment.getAdditionalDetails().getExemptionCodes());
                }

                if(shipment.getAdditionalDetails().getScreeningStatus() != null ) {
                    screeningStatus = String.join("+",
                        shipment.getAdditionalDetails().getScreeningStatus().stream()
                            // Ignore the NOT screening status from the result set
                            .filter(i -> !AwbConstants.NOT_SCREENING_STATUS.equalsIgnoreCase(i))
                            .map(i -> {
                                var res = i;
                                // Add the free text value whenever AOM is selected as screening status
                                if (AwbConstants.AOM_SCREENING_STATUS.equalsIgnoreCase(i))
                                    res += String.format(" (%s)", shipment.getAdditionalDetails().getAomFreeText());
                                return res;
                        }).toList()
                    );
                }

                csdInfo = String.format(AwbConstants.CSD_INFO_FORMAT,raNumber, securityStatus, screeningStatus);
            }
        }
        return csdInfo;
    }

    private String getCountryCode(String country) {
        CommonV1ListRequest request = new CommonV1ListRequest();
        List<Object> criteria = new ArrayList<>();
        List<Object> subCriteria1 = Arrays.asList(
                Arrays.asList(MasterDataConstants.ITEM_TYPE),
                "=",
                MasterDataType.COUNTRIES.getId()
        );
        List<Object> subCriteria2 = Arrays.asList(
                Arrays.asList(MasterDataConstants.ITEM_VALUE),
                "=",
                country
        );
        criteria.addAll(List.of(subCriteria1, "and", subCriteria2));
        request.setCriteriaRequests(criteria);
        try {
            V1DataResponse response = v1Service.fetchMasterData(request);
            List<EntityTransferMasterLists> responseList = jsonHelper.convertValueToList(response.entities, EntityTransferMasterLists.class);
            if (responseList != null && responseList.size() > 0)
                return responseList.get(0).getIdentifier1();
        } catch (Exception ignored) {
        }
        return null;
    }

    private AwbOtherInfo generateAwbOtherInfo(ShipmentDetails shipmentDetails, CreateAwbRequest request) {
        AwbOtherInfo awbOtherInfo = new AwbOtherInfo();
        awbOtherInfo.setEntityId(shipmentDetails.getId());
        awbOtherInfo.setEntityType(request.getAwbType());
        var shipperName = StringUtility.convertToString(shipmentDetails.getConsigner() != null ? shipmentDetails.getConsigner().getOrgData().get(PartiesConstants.FULLNAME) : "");
        awbOtherInfo.setShipper(shipperName == null ? null : shipperName.toUpperCase());
        awbOtherInfo.setExecutedOn(jsonHelper.convertValue(DateTimeFormatter.ofPattern(Constants.YYYY_MM_DD_T_HH_MM_SS).format(LocalDateTime.now()), LocalDateTime.class));
        awbOtherInfo.setExecutedAt(executedAt);
        getAwbOtherInfoMasterData(awbOtherInfo, request.getAwbType());
        return awbOtherInfo;
    }

    private List<AwbGoodsDescriptionInfo> generateAwbGoodsDescriptionInfo(ShipmentDetails shipmentDetails, CreateAwbRequest request, List<AwbPackingInfo> awbPackingList) {
        AwbGoodsDescriptionInfo awbGoodsDescriptionInfo = new AwbGoodsDescriptionInfo();
        awbGoodsDescriptionInfo.setEntityId(shipmentDetails.getId());
        awbGoodsDescriptionInfo.setEntityType(request.getAwbType());
        awbGoodsDescriptionInfo.setIsShipmentCreated(true);
        awbGoodsDescriptionInfo.setGrossWt(shipmentDetails.getWeight());
        awbGoodsDescriptionInfo.setGrossWtUnit(shipmentDetails.getWeightUnit());
        awbGoodsDescriptionInfo.setPiecesNo(shipmentDetails.getNoOfPacks());
        awbGoodsDescriptionInfo.setChargeableWt(shipmentDetails.getChargable() != null ?
                AwbUtility.roundOffAirShipment((double) shipmentDetails.getChargable().doubleValue()) : null);
        awbGoodsDescriptionInfo.setGuid(UUID.randomUUID());
        int noOfPacks = 0;
        if(awbPackingList != null) {
            for (var awbPacking: awbPackingList ) {
                awbPacking.setAwbGoodsDescriptionInfoGuid(awbGoodsDescriptionInfo.getGuid());
                noOfPacks += Integer.parseInt(Objects.isNull(awbPacking.getPacks()) ? "0" : awbPacking.getPacks());
            }
            awbGoodsDescriptionInfo.setPiecesNo(noOfPacks);
        }
        awbGoodsDescriptionInfo.setAwbPackingInfo(awbPackingList);
        return Arrays.asList(awbGoodsDescriptionInfo);
    }

    private List<AwbPackingInfo> generateAwbPackingInfo(ShipmentDetails shipmentDetails, List<Packing> packings) {
        Map<Long, String> map = new HashMap<>();
        if(shipmentDetails.getContainersList() != null && shipmentDetails.getContainersList().size() > 0)
            map = shipmentDetails.getContainersList().stream().collect(Collectors.toMap(Containers::getId, Containers::getContainerNumber));
        if (packings != null && packings.size() > 0) {
            List<AwbPackingInfo> awbPackingList = new ArrayList<>();
            for (var packing : packings) {
                AwbPackingInfo awbPacking = new AwbPackingInfo();
                awbPacking.setGuid(packing.getGuid());
                awbPacking.setDgGoodsId(packing.getDGGoodsId());
                awbPacking.setDgSubstanceId(packing.getDGSubstanceId());
                awbPacking.setPacks(packing.getPacks());
                awbPacking.setPacksType(packing.getPacksType());
                if(packing.getContainerId() != null && map.containsKey(packing.getContainerId()))
                    awbPacking.setContainerNumber(map.get(packing.getContainerId()));
                awbPacking.setWeight(packing.getWeight());
                awbPacking.setWeightUnit(packing.getWeightUnit());
                awbPacking.setVolume(packing.getVolume());
                awbPacking.setVolumeUnit(packing.getVolumeUnit());
                awbPacking.setInspections(packing.getInspections());
                awbPacking.setOrigin(packing.getOrigin());
                awbPacking.setCommodity(packing.getCommodity());
                awbPacking.setPackingOrder(packing.getPackingOrder());
                awbPacking.setLength(packing.getLength());
                awbPacking.setLengthUnit(packing.getLengthUnit());
                awbPacking.setWidth(packing.getWidth());
                awbPacking.setWidthUnit(packing.getWidthUnit());
                awbPacking.setHeight(packing.getHeight());
                awbPacking.setHeightUnit(packing.getHeightUnit());
                awbPacking.setMarksnNums(packing.getMarksnNums());
                awbPacking.setFlashPoint(packing.getFlashPoint());
                awbPacking.setUndgContact(packing.getUNDGContact());
                awbPacking.setIsTemperatureControlled(packing.getIsTemperatureControlled());
                awbPacking.setMinTemp(packing.getMinTemp());
                awbPacking.setMinTempUnit(packing.getMinTempUnit());
                awbPacking.setHsCode(packing.getHSCode());
                awbPacking.setCountryCode(packing.getCountryCode());
                awbPacking.setGoodsDescription(packing.getGoodsDescription() == null ? null : packing.getGoodsDescription().toUpperCase());
                awbPacking.setReferenceNumber(packing.getReferenceNumber());
                awbPacking.setDgClass(packing.getDGClass());
                awbPacking.setHazardous(packing.getHazardous());
                awbPacking.setNetWeight(packing.getNetWeight());
                awbPacking.setNetWeightUnit(packing.getNetWeightUnit());
                awbPacking.setVolumeWeight(packing.getVolumeWeight());
                awbPacking.setVolumeWeightUnit(packing.getVolumeWeightUnit());
                awbPacking.setUnNumberAir(packing.getUnNumberAir());
                awbPacking.setDgClassAir(packing.getDgClassAir());
                awbPacking.setDgClassAirDescription(packing.getDgClassAirDescription());
                awbPacking.setAwbNumber(shipmentDetails.getHouseBill());
                totalPacks += Integer.parseInt(packing.getPacks());
                awbPackingList.add(awbPacking);
            }

            return awbPackingList;
        }
        return null;
    }

    public ResponseEntity<IRunnerResponse> createV1Awb(CommonRequestModel commonRequestModel, boolean checkForSync){
        return ResponseHelper.buildSuccessResponse();
    }

//    private void setEntityId(Awb request, Long entityId){
//        if(request.getAwbShipmentInfo() != null)
//            request.getAwbShipmentInfo().setEntityId(entityId);
//        if(request.getAwbNotifyPartyInfo() != null)
//            request.getAwbNotifyPartyInfo().forEach(i -> i.setEntityId(entityId) );
//        if(request.getAwbRoutingInfo() != null)
//            request.getAwbRoutingInfo().forEach(i -> i.setEntityId(entityId) );
//        if(request.getAwbCargoInfo() != null)
//            request.getAwbCargoInfo().setEntityId(entityId);
//        if(request.getAwbPaymentInfo() != null)
//            request.getAwbPaymentInfo().setEntityId(entityId);
//        if(request.getAwbOtherChargesInfo() != null)
//            request.getAwbOtherChargesInfo().forEach(i -> i.setEntityId(entityId) );
//        if(request.getAwbOtherInfo() != null)
//            request.getAwbOtherInfo().setEntityId(entityId);
//        if(request.getAwbOciInfo() != null)
//            request.getAwbOciInfo().forEach(i -> i.setEntityId(entityId) );
//        if(request.getAwbGoodsDescriptionInfo() != null)
//            request.getAwbGoodsDescriptionInfo().forEach(i -> i.setEntityId(entityId) );
//        if(request.getAwbSpecialHandlingCodesMappings() != null)
//            request.getAwbSpecialHandlingCodesMappings().forEach(i -> i.setEntityId(entityId) );
//    }

    private void updateAwbOtherChargesInfo(List<AwbOtherChargesInfo> otherChargesInfos) {
        if(otherChargesInfos != null && otherChargesInfos.size() > 0) {
            otherChargesInfos.stream().map(i -> {
                if(i.getGuid() == null)
                    i.setGuid(UUID.randomUUID());
                return i;
            }).toList();
        }
    }

    List<Awb>  getLinkedAwbFromMawb(Long mawbId) {
        List<MawbHawbLink> mawbHawbLinks = mawbHawbLinkDao.findByMawbId(mawbId);

        // Fetch all the awb records with the mapped hawbId
        return awbDao.findByIds(mawbHawbLinks.stream().map(MawbHawbLink::getHawbId).toList());
    }


    @Async
    private void callV1Sync(Awb entity, SaveStatus saveStatus){
        awbSync.sync(entity, saveStatus);
    }

    public ResponseEntity<IRunnerResponse> customAwbRetrieve(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CustomAwbRetrieveRequest request = (CustomAwbRetrieveRequest) commonRequestModel.getData();
            List<String> awbNumber = request.getAwbNumber();
            String issuingAgentName = request.getIssuingAgent();
            List<Awb> awbs = new ArrayList<>();
            if (awbNumber == null && issuingAgentName == null)
                log.error(AwbConstants.AWB_RETRIEVE_REQUEST_NULL_ERROR, LoggerHelper.getRequestIdFromMDC());
            else if (issuingAgentName == null)
                awbs = awbDao.findByAwbNumber(awbNumber);
            else if (awbNumber == null)
                awbs = awbDao.findByIssuingAgent(issuingAgentName);
            else
                awbs = awbDao.findByAwbNumberAndIssuingAgent(awbNumber, issuingAgentName);
            return ResponseHelper.buildSuccessResponse(jsonHelper.convertValueToList(awbs, AwbResponse.class));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    @Transactional
    public ResponseEntity<IRunnerResponse> reset(CommonRequestModel commonRequestModel) throws RunnerException {
        ResetAwbRequest resetAwbRequest = (ResetAwbRequest) commonRequestModel.getData();
        Optional<Awb> awbOptional = awbDao.findById(resetAwbRequest.getId());

        if (awbOptional.isEmpty()) {
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        Awb awb = awbOptional.get();
        UUID awbGuid = awb.getGuid();
        Long awbId = awb.getId();
        AwbStatus airMessageStatus = awb.getAirMessageStatus();
        AwbStatus linkedHawbAirMessageStatus = awb.getLinkedHawbAirMessageStatus();
        PrintType printType = awb.getPrintType();
        LocalDateTime originalPrintedAt = awb.getOriginalPrintedAt();
        var isAirMessagingSent = false; //awb.getIsAirMessagingSent();

        Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(awb.getShipmentId());
        Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findById(awb.getConsolidationId());

        if (shipmentDetails.isEmpty() && consolidationDetails.isEmpty())
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);

        CreateAwbRequest createAwbRequest = CreateAwbRequest.builder()
                .ConsolidationId(resetAwbRequest.getConsolidationId())
                .ShipmentId(resetAwbRequest.getShipmentId())
                .AwbType(resetAwbRequest.getAwbType())
                .isReset(true)
                .build();
        switch (resetAwbRequest.getResetType()) {
            case ALL: {
                if(resetAwbRequest.getAwbType().equals(Constants.MAWB)) {
                    List<Awb> awbList = new ArrayList<>();
                    List<AwbPackingInfo> mawbPackingInfo = new ArrayList<>();
                    for (var consoleShipment : consolidationDetails.get().getShipmentsList()) {
                        if (consoleShipment.getId() != null) {
                            Optional<Awb> linkAwb = awbDao.findByShipmentIdList(Arrays.asList(consoleShipment.getId())).stream().findFirst();
                            if (linkAwb.isEmpty()) {
                                throw new ValidationException(AwbConstants.GENERATE_HAWB_BEFORE_MAWB_EXCEPTION);
                            }
                            awbList.add(linkAwb.get());
                            if(linkAwb.get().getAwbPackingInfo() != null) {
                                mawbPackingInfo.addAll(linkAwb.get().getAwbPackingInfo());
                            }
                        }
                    }
                    Awb resetAwb = generateMawb(createAwbRequest, consolidationDetails.get(), mawbPackingInfo);
                    awb.setAwbShipmentInfo(resetAwb.getAwbShipmentInfo());
                    awb.setAwbNotifyPartyInfo(resetAwb.getAwbNotifyPartyInfo());
                    awb.setAwbRoutingInfo(resetAwb.getAwbRoutingInfo());
                    awb.setAwbGoodsDescriptionInfo(resetAwb.getAwbGoodsDescriptionInfo());
                    awb.setAwbCargoInfo(resetAwb.getAwbCargoInfo());
                    awb.setAwbOtherInfo(resetAwb.getAwbOtherInfo());
                    awb.setAwbOciInfo(resetAwb.getAwbOciInfo());
                    awb.setAwbOtherChargesInfo(resetAwb.getAwbOtherChargesInfo());
                    awb.setAwbPaymentInfo(resetAwb.getAwbPaymentInfo());
                    awb.setAwbSpecialHandlingCodesMappings(resetAwb.getAwbSpecialHandlingCodesMappings());
                    // Link
                    LinkHawbMawb(awb, awbList);
                    awb.setPrintType(printType);
                    if(awbList != null && !awbList.isEmpty())
                        updateSciFieldFromMawb(awb, awbList);
                }
                else {
                    awb = generateAwb(createAwbRequest);
                    awb.setPrintType(printType);
                    awbDao.updateSciFieldFromHawb(awb, null, true, awbId);
                }
                awb.setGuid(awbGuid);
                awb.setAirMessageStatus(airMessageStatus);
                awb.setLinkedHawbAirMessageStatus(linkedHawbAirMessageStatus);
                break;
            }
            case AWB_ROUTING: {
                if(resetAwbRequest.getAwbType().equals(Constants.MAWB))
                    awb.setAwbRoutingInfo(generateMawbRoutingInfo(consolidationDetails.get(), createAwbRequest));
                else awb.setAwbRoutingInfo(generateAwbRoutingInfo(shipmentDetails.get(), createAwbRequest));
                break;
            }
            case AWB_NOTIFY_PARTY_INFO: {
                if(resetAwbRequest.getAwbType().equals(Constants.MAWB))
                    awb.setAwbNotifyPartyInfo(generateMawbNotifyPartyinfo(consolidationDetails.get(), createAwbRequest));
                else
                    awb.setAwbNotifyPartyInfo(generateAwbNotifyPartyinfo(shipmentDetails.get(), createAwbRequest));
                break;
            }
            case AWB_PACKS_AND_GOODS: {
                if (resetAwbRequest.getAwbType().equals(Constants.MAWB)) {
                    awb.setAwbGoodsDescriptionInfo(generateMawbGoodsDescriptionInfo(consolidationDetails.get(), createAwbRequest, null));
                    updateLinkHawbMawb(consolidationDetails.get(), awbId);
                }
                else {
                    awb.setAwbPackingInfo(generateAwbPackingInfo(shipmentDetails.get(), shipmentDetails.get().getPackingList()));
                    awb.setAwbGoodsDescriptionInfo(generateAwbGoodsDescriptionInfo(shipmentDetails.get(), createAwbRequest, awb.getAwbPackingInfo()));
                }
                break;
            }
            case AWB_OTHER_CHARGES_INFO: {
                awb.setAwbOtherChargesInfo(null);
                awb.setAwbPaymentInfo(null);
                break;
            }
            case AWB_OCI_INFO: {
                awb.setAwbOciInfo(null);
                break;
            }
        }
        awb.setId(resetAwbRequest.getId());
        awb.setAirMessageResubmitted(false);
        awb.setOriginalPrintedAt(originalPrintedAt);
        awb = awbDao.save(awb);
        try {
            callV1Sync(awb, SaveStatus.RESET);
        } catch (Exception e) {
            log.error(SyncingConstants.ERROR_PERFORMING_AWB_SYNC, e);
        }

        return ResponseHelper.buildSuccessResponse(convertEntityToDto(awb));
    }

    @Override
    public ResponseEntity<IRunnerResponse> partialAutoUpdateAwb(CommonRequestModel commonRequestModel) throws RunnerException {
        String responseMsg;
        CreateAwbRequest request = (CreateAwbRequest) commonRequestModel.getData();
        if (request == null) {
            log.debug("Request is empty for AWB Create for Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new ValidationException("Request can't be null");
        }

        if (request.getShipmentId() == null) {
            log.error("Shipment Id can't be null or empty in create AWB Request");
            throw new ValidationException("Shipment Id can't be null or empty in Create AWB Request");
        }

        List<Awb> awbs = awbDao.findByShipmentId(request.getShipmentId());
        if(awbs.isEmpty()){
            log.error("No Awb exist for given shipment to update");
            throw new ValidationException("No Awb exist for given shipment to update");
        }

        Awb awb = awbs.get(0);

        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();

        if(shipmentSettingsDetails.getRestrictAWBEdit() != null && shipmentSettingsDetails.getRestrictAWBEdit()){
            ResetAwbRequest resetAwbRequest = ResetAwbRequest.builder()
                    .id(awb.getId())
                    .shipmentId(request.getShipmentId())
                    .consolidationId(request.getConsolidationId())
                    .awbType(request.getAwbType())
                    .resetType(AwbReset.ALL)
                    .build();
            return this.reset(CommonRequestModel.buildRequest(resetAwbRequest));
        }
        else if(shipmentSettingsDetails.getAutoUpdateShipmentAWB() != null && shipmentSettingsDetails.getAutoUpdateShipmentAWB()) {
            updateAwbFromShipment(awb, request, shipmentSettingsDetails);
            awb = awbDao.save(awb);
        }
        log.info("AWB created successfully for Id {} with Request Id {}", awb.getId(), LoggerHelper.getRequestIdFromMDC());
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(awb));
    }

    private void updateAwbFromShipment(Awb awb, CreateAwbRequest request, ShipmentSettingsDetails shipmentSettingsDetails) {
        ShipmentDetails shipmentDetails = shipmentDao.findById(request.getShipmentId()).get();

        // fetch all packings
        List<Packing> packings = shipmentDetails.getPackingList();



        HawbLockSettings hawbLockSettings = shipmentSettingsDetails.getHawbLockSettings();
        MawbLockSettings mawbLockSettings = shipmentSettingsDetails.getMawbLockSettings();

        Integer totalPacksCount = 0;
        updateShipmentPackingInfoToAwb(shipmentDetails, packings, awb, request, hawbLockSettings, mawbLockSettings, totalPacksCount);
        updateShipmentInfoToAwb(shipmentDetails, request, awb, hawbLockSettings, mawbLockSettings);
        updateShipmentNotifyPartyinfoToAwb(shipmentDetails, request, awb, hawbLockSettings, mawbLockSettings);
        updateShipmemtRoutingInfoToAwb(shipmentDetails, request, awb, hawbLockSettings, mawbLockSettings);
        updateAwbGoodsDescriptionInfoFromShipment(shipmentDetails, request, awb, hawbLockSettings, mawbLockSettings);
        updateAwbCargoInfoFromShipment(shipmentDetails, request, awb, hawbLockSettings, mawbLockSettings);
        updateAwbOtherInfoFromShipment(shipmentDetails, request, awb, hawbLockSettings, mawbLockSettings);
    }

    private void updateShipmentPackingInfoToAwb(ShipmentDetails shipmentDetails, List<Packing> packings, Awb awb, CreateAwbRequest request, HawbLockSettings hawbLockSettings, MawbLockSettings mawbLockSettings, Integer totalPacksCount) {
        Map<UUID, Packing> packMap = new HashMap<>();
        packings.forEach(pack -> {
            packMap.put(pack.getGuid(), pack);
        });
        List<AwbPackingInfo> deletedList = new ArrayList<>();
        if(awb.getAwbPackingInfo() != null && !awb.getAwbPackingInfo().isEmpty()) {
            for (var cargo : awb.getAwbPackingInfo()) {
                if (cargo.getGuid() != null) {
                    if (packMap.containsKey(cargo.getGuid())) {
                        updateShipmentPackingFieldToHbl(packMap.get(cargo.getGuid()), cargo, request, hawbLockSettings, mawbLockSettings);
                        totalPacksCount += Integer.parseInt(cargo.getPacks());
                        packMap.remove(cargo.getGuid());
                    } else {
                        deletedList.add(cargo);
                    }
                } else {
                    totalPacksCount += Integer.parseInt(cargo.getPacks());
                }
            }
            awb.getAwbPackingInfo().removeAll(deletedList);
        }

        Map<Long, String> map = new HashMap<>();
        if(shipmentDetails.getContainersList() != null && shipmentDetails.getContainersList().size() > 0)
            map = shipmentDetails.getContainersList().stream().collect(Collectors.toMap(Containers::getId, Containers::getContainerNumber));

        if(!packMap.isEmpty()) {
            for (var packing: packMap.values()) {
                AwbPackingInfo awbPacking = new AwbPackingInfo();
                awbPacking.setGuid(packing.getGuid());
                awbPacking.setDgGoodsId(packing.getDGGoodsId());
                awbPacking.setDgSubstanceId(packing.getDGSubstanceId());
                awbPacking.setPacks(packing.getPacks());
                awbPacking.setPacksType(packing.getPacksType());
                if(packing.getContainerId() != null && map.containsKey(packing.getContainerId()))
                    awbPacking.setContainerNumber(map.get(packing.getContainerId()));
                awbPacking.setWeight(packing.getWeight());
                awbPacking.setWeightUnit(packing.getWeightUnit());
                awbPacking.setVolume(packing.getVolume());
                awbPacking.setVolumeUnit(packing.getVolumeUnit());
                awbPacking.setInspections(packing.getInspections());
                awbPacking.setOrigin(packing.getOrigin());
                awbPacking.setCommodity(packing.getCommodity());
                awbPacking.setPackingOrder(packing.getPackingOrder());
                awbPacking.setLength(packing.getLength());
                awbPacking.setLengthUnit(packing.getLengthUnit());
                awbPacking.setWidth(packing.getWidth());
                awbPacking.setWidthUnit(packing.getWidthUnit());
                awbPacking.setHeight(packing.getHeight());
                awbPacking.setHeightUnit(packing.getHeightUnit());
                awbPacking.setMarksnNums(packing.getMarksnNums());
                awbPacking.setFlashPoint(packing.getFlashPoint());
                awbPacking.setUndgContact(packing.getUNDGContact());
                awbPacking.setIsTemperatureControlled(packing.getIsTemperatureControlled());
                awbPacking.setMinTemp(packing.getMinTemp());
                awbPacking.setMinTempUnit(packing.getMinTempUnit());
                awbPacking.setHsCode(packing.getHSCode());
                awbPacking.setCountryCode(packing.getCountryCode());
                awbPacking.setGoodsDescription(packing.getGoodsDescription() == null ? null : packing.getGoodsDescription().toUpperCase());
                awbPacking.setReferenceNumber(packing.getReferenceNumber());
                awbPacking.setDgClass(packing.getDGClass());
                awbPacking.setHazardous(packing.getHazardous());
                awbPacking.setNetWeight(packing.getNetWeight());
                awbPacking.setNetWeightUnit(packing.getNetWeightUnit());
                awbPacking.setVolumeWeight(packing.getVolumeWeight());
                awbPacking.setVolumeWeightUnit(packing.getVolumeWeightUnit());
                awbPacking.setUnNumberAir(packing.getUnNumberAir());
                awbPacking.setDgClassAir(packing.getDgClassAir());
                awbPacking.setDgClassAirDescription(packing.getDgClassAirDescription());
                awbPacking.setAwbNumber(shipmentDetails.getHouseBill());
                totalPacksCount += Integer.parseInt(packing.getPacks());
                if(awb.getAwbPackingInfo() == null){
                    awb.setAwbPackingInfo(new ArrayList<>());
                }
                awb.getAwbPackingInfo().add(awbPacking);
            }
        }
    }
    private void updateShipmentPackingFieldToHbl(Packing packing, AwbPackingInfo awbPackingInfo, CreateAwbRequest request, HawbLockSettings hawbLockSettings, MawbLockSettings mawbLockSettings) {
        if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getPackingPacksLock()) ||
                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getPackingPacksLock()))
            awbPackingInfo.setPacks(packing.getPacks());
        if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getPackingPacksTypeLock()) ||
                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getPackingPacksTypeLock()))
            awbPackingInfo.setPacksType(packing.getPacksType());
        if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getPackingOriginLock()) ||
                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getPackingOriginLock()))
            awbPackingInfo.setOrigin(packing.getOrigin());
        if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getPackingOrderLock()) ||
                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getPackingOrderLock()))
            awbPackingInfo.setPackingOrder(packing.getPackingOrder());
        if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getPackingLengthLock()) ||
                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getPackingLengthLock()))
            awbPackingInfo.setLength(packing.getLength());
        if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getPackingLengthUnitLock()) ||
                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getPackingLengthUnitLock()))
            awbPackingInfo.setLengthUnit(packing.getLengthUnit());
        if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getPackingWidthLock()) ||
                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getPackingWidthLock()))
            awbPackingInfo.setWidth(packing.getWidth());
        if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getPackingWidthUnitLock()) ||
                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getPackingWidthUnitLock()))
            awbPackingInfo.setWidthUnit(packing.getWidthUnit());
        if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getPackingHeightLock()) ||
                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getPackingHeightLock()))
            awbPackingInfo.setHeight(packing.getHeight());
        if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getPackingHeightUnitLock()) ||
                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getPackingHeightUnitLock()))
            awbPackingInfo.setHeightUnit(packing.getHeightUnit());
        if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getPackingWeightLock()) ||
                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getPackingWeightLock()))
            awbPackingInfo.setWeight(packing.getWeight());
        if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getPackingWeightUnitLock()) ||
                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getPackingWeightUnitLock()))
            awbPackingInfo.setWeightUnit(packing.getWeightUnit());
        if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getPackingVolumeLock()) ||
                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getPackingVolumeLock()))
            awbPackingInfo.setVolume(packing.getVolume());
        if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getPackingVolumeUnitLock()) ||
                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getPackingVolumeUnitLock()))
            awbPackingInfo.setVolumeUnit(packing.getVolumeUnit());
        if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getPackingNetWeightLock()) ||
                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getPackingNetWeightLock()))
            awbPackingInfo.setNetWeight(packing.getNetWeight());
        if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getPackingNetWeightUnitLock()) ||
                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getPackingNetWeightUnitLock()))
            awbPackingInfo.setNetWeightUnit(packing.getNetWeightUnit());
        if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getPackingVolumeWeightLock()) ||
                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getPackingVolumeWeightLock()))
            awbPackingInfo.setVolumeWeight(packing.getVolumeWeight());
        if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getPackingVolumeWeightUnitLock()) ||
                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getPackingVolumeWeightUnitLock()))
            awbPackingInfo.setVolumeWeightUnit(packing.getVolumeWeightUnit());
        if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getPackingMarksnNumsLock()) ||
                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getPackingMarksnNumsLock()))
            awbPackingInfo.setMarksnNums(packing.getMarksnNums());
        if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getPackingCountryCodeLock()) ||
                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getPackingCountryCodeLock()))
            awbPackingInfo.setCountryCode(packing.getCountryCode());
        if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getPackingGoodsDescLock()) ||
                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getPackingGoodsDescLock()))
            awbPackingInfo.setGoodsDescription(packing.getGoodsDescription() == null ? null : packing.getGoodsDescription().toUpperCase());
        if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getPackingReferenceNumberLock()) ||
                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getPackingReferenceNumberLock()))
            awbPackingInfo.setReferenceNumber(packing.getReferenceNumber());
        if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getPackingInspectionsLock()) ||
                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getPackingInspectionsLock()))
            awbPackingInfo.setInspections(packing.getInspections());
        if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getPackingDgClassLock()) ||
                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getPackingDgClassLock()))
            awbPackingInfo.setDgClass(packing.getDGClass());
        if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getPackingDgSubstanceIdLock()) ||
                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getPackingDgSubstanceIdLock()))
            awbPackingInfo.setDgSubstanceId(packing.getDGSubstanceId());
        if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getPackingMinTempLock()) ||
                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getPackingMinTempLock()))
            awbPackingInfo.setMinTemp(packing.getMinTemp());
        if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getPackingMinTempUnitLock()) ||
                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getPackingMinTempUnitLock()))
            awbPackingInfo.setMinTempUnit(packing.getMinTempUnit());
        if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getPackingMaxTempLock()) ||
                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getPackingMaxTempLock()))
            awbPackingInfo.setMaxTemp(packing.getMaxTemp());
        if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getPackingMaxTempUnitLock()) ||
                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getPackingMaxTempUnitLock()))
            awbPackingInfo.setMaxTempUnit(packing.getMaxTempUnit());
        if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getPackingCommodityLock()) ||
                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getPackingCommodityLock()))
            awbPackingInfo.setCommodity(packing.getCommodity());
        if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getPackingHsCodeLock()) ||
                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getPackingHsCodeLock()))
            awbPackingInfo.setHsCode(packing.getHSCode());
        awbPackingInfo.setUnNumberAir(packing.getUnNumberAir());
        awbPackingInfo.setDgClassAir(packing.getDgClassAir());
        awbPackingInfo.setDgClassAirDescription(packing.getDgClassAirDescription());
    }

    private void updateShipmentInfoToAwb(ShipmentDetails shipmentDetails, CreateAwbRequest request, Awb awb, HawbLockSettings hawbLockSettings, MawbLockSettings mawbLockSettings) {
        AwbShipmentInfo awbShipmentInfo = awb.getAwbShipmentInfo();
        if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getAwbNumberLock()) ||
                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getAwbNumberLock()))
            awbShipmentInfo.setAwbNumber(shipmentDetails.getHouseBill());
        if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getShipperNameLock()) ||
                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getShipperNameLock())) {
            var shipperName = StringUtility.convertToString(shipmentDetails.getConsigner() != null ? shipmentDetails.getConsigner().getOrgData().get(PartiesConstants.FULLNAME) : "");
            awbShipmentInfo.setShipperName(shipperName == null ? shipperName : shipperName.toUpperCase());
        }
        if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getShipperAddressLock()) ||
                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getShipperAddressLock())) {
            var shipperAddress = AwbUtility.constructAddress(shipmentDetails.getConsigner() != null ? shipmentDetails.getConsigner().getAddressData() : null);
            awbShipmentInfo.setShipperAddress(shipperAddress == null ? shipperAddress : shipperAddress.toUpperCase());
        }
        if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getConsigneeNameLock()) ||
                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getConsigneeNameLock())) {
            var consigneeName = StringUtility.convertToString(shipmentDetails.getConsignee() != null ? shipmentDetails.getConsignee().getOrgData().get(PartiesConstants.FULLNAME) : "");
            awbShipmentInfo.setConsigneeName(consigneeName == null ? consigneeName : consigneeName.toUpperCase());
        }
        if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getConsigneeAddressLock()) ||
                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getConsigneeAddressLock())) {
            var consigneeAddress = AwbUtility.constructAddress(shipmentDetails.getConsignee().getAddressData());
            awbShipmentInfo.setConsigneeAddress(consigneeAddress == null ? consigneeAddress : consigneeAddress.toUpperCase());
        }

        boolean originUpdate = (request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getAirportOfDepartureLock()) ||
                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getAirportOfDepartureLock());

        boolean destinationUpdate = (request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getAirportOfDestinationLock()) ||
                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getAirportOfDestinationLock());

        if(originUpdate || destinationUpdate) {
            setAwbShipmentInfoUnLocationData(awbShipmentInfo, shipmentDetails.getCarrierDetails(), !originUpdate, !destinationUpdate);
        }
        if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getFirstCarrierLock()) ||
                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getFirstCarrierLock()))
            awbShipmentInfo.setFirstCarrier(shipmentDetails.getCarrierDetails().getShippingLine());
        if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getIataCodeLock()) ||
                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getIataCodeLock()))
            awbShipmentInfo.setIataCode(iataCode);


        for (var orgRow : shipmentDetails.getShipmentAddresses()) {
            if (orgRow.getType().equals(Constants.FAG)) {
                if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getIssuingAgentNameLock()) ||
                        (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getIssuingAgentNameLock())) {
                    var issuingAgentName = StringUtility.convertToString(orgRow.getOrgData().get(PartiesConstants.FULLNAME));
                    awbShipmentInfo.setIssuingAgentName(issuingAgentName == null ? issuingAgentName : issuingAgentName.toUpperCase()); // extract from orgdata
                }
                if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getIssuingAgentAddressLock()) ||
                        (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getIssuingAgentAddressLock())) {
                    var issuingAgentAddress = AwbUtility.constructAddress(orgRow.getAddressData());
                    awbShipmentInfo.setIssuingAgentAddress(issuingAgentAddress == null ? issuingAgentAddress : issuingAgentAddress.toUpperCase());
                }

                awbShipmentInfo.setIataCode(StringUtility.isEmpty(awbShipmentInfo.getIataCode())
                        ? StringUtility.convertToString(shipmentDetails.getConsignee().getOrgData().get(PartiesConstants.AGENT_IATA_CODE))
                        : awbShipmentInfo.getIataCode());
                awbShipmentInfo.setAgentCASSCode(StringUtility.isEmpty(awbShipmentInfo.getAgentCASSCode())
                        ? StringUtility.convertToString(shipmentDetails.getConsignee().getOrgData().get(PartiesConstants.AGENT_CASS_CODE))
                        : awbShipmentInfo.getAgentCASSCode());
            }
        }
    }
    private void updateShipmentNotifyPartyinfoToAwb(ShipmentDetails shipmentDetails, CreateAwbRequest request, Awb awb, HawbLockSettings hawbLockSettings, MawbLockSettings mawbLockSettings) {
        var party = shipmentDetails.getAdditionalDetails().getNotifyParty();
        boolean createNotifyParty = true;
        AwbNotifyPartyInfo deleteParty = new AwbNotifyPartyInfo();
        if(awb.getAwbNotifyPartyInfo() != null && !awb.getAwbNotifyPartyInfo().isEmpty()) {
            for (var awbParty : awb.getAwbNotifyPartyInfo()) {
                if (awbParty.getIsShipmentCreated() != null && awbParty.getIsShipmentCreated()) {
                    createNotifyParty = false;
                    if (party != null) {
                        if ((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getNotifyOrganizationLock()) ||
                                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getNotifyOrganizationLock())) {
                            var name = StringUtility.convertToString(party.getOrgData().get(PartiesConstants.FULLNAME));
                            awbParty.setName(name == null ? name : name.toUpperCase());
                            awbParty.setNotifyOrgId(party.getId());
                        }
                        if ((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getNotifyOrganizationAddressLock()) ||
                                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getNotifyOrganizationAddressLock())) {
                            awbParty.setAddress(AwbUtility.constructAddress(party.getAddressData()).toUpperCase());
                        }

                    } else {
                        deleteParty = awbParty;
                    }
                }
            }
            awb.getAwbNotifyPartyInfo().remove(deleteParty);
        }

        AwbNotifyPartyInfo awbParty = AwbNotifyPartyInfo.builder().build();
        if (party != null && createNotifyParty) {
            awbParty.setIsShipmentCreated(true);
            var name = StringUtility.convertToString(party.getOrgData().get(PartiesConstants.FULLNAME));
            awbParty.setName(name == null ? name : name.toUpperCase());
            awbParty.setAddress(AwbUtility.constructAddress(party.getAddressData()).toUpperCase());
            awbParty.setEntityId(shipmentDetails.getId());
            awbParty.setEntityType(request.getAwbType());
            awbParty.setNotifyOrgId(party.getId());
            if(awb.getAwbNotifyPartyInfo() == null){
                awb.setAwbNotifyPartyInfo(new ArrayList<>());
            }
            awb.getAwbNotifyPartyInfo().add(awbParty);
        }
    }
    private void updateShipmemtRoutingInfoToAwb(ShipmentDetails shipmentDetails, CreateAwbRequest request, Awb awb, HawbLockSettings hawbLockSettings, MawbLockSettings mawbLockSettings) {
        boolean createRouting = true;
        AwbRoutingInfo deleteParty = new AwbRoutingInfo();
        if(awb.getAwbRoutingInfo() != null && !awb.getAwbRoutingInfo().isEmpty()) {
            for (var awbRoute : awb.getAwbRoutingInfo()) {
                if (awbRoute.getIsShipmentCreated() != null && awbRoute.getIsShipmentCreated()) {
                    createRouting = false;
                    if (shipmentDetails.getCarrierDetails() != null &&
                            shipmentDetails.getCarrierDetails().getOriginPort() != null &&
                            shipmentDetails.getCarrierDetails().getDestinationPort() != null) {
                        if ((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getOriginPortLock()) ||
                                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getOriginPortLock())) {
                            awbRoute.setOriginPortName(shipmentDetails.getCarrierDetails().getOriginPort());
                        }
                        if ((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getDestinationPortLock()) ||
                                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getDestinationPortLock())) {
                            awbRoute.setDestinationPortName(shipmentDetails.getCarrierDetails().getDestinationPort());
                        }
                        if ((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getByCarrierLock()) ||
                                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getByCarrierLock()))
                            awbRoute.setByCarrier(shipmentDetails.getCarrierDetails().getShippingLine());
                        if ((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getFlightNumberLock()) ||
                                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getFlightNumberLock()))
                            awbRoute.setFlightNumber(shipmentDetails.getCarrierDetails().getFlightNumber());
                        if ((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getFlightDateLock()) ||
                                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getFlightDateLock())) {
                            var flightDate = request.getAwbType() == Constants.DMAWB ? shipmentDetails.getCarrierDetails().getEtd() : null;
                            awbRoute.setFlightDate(flightDate);
                        }
                    } else {
                        deleteParty = awbRoute;
                    }
                }
            }
            awb.getAwbRoutingInfo().remove(deleteParty);
        }

        if (shipmentDetails.getCarrierDetails() != null &&
                shipmentDetails.getCarrierDetails().getOriginPort() != null &&
                shipmentDetails.getCarrierDetails().getDestinationPort() != null && createRouting
        ) {
            var flightDate = request.getAwbType() == Constants.DMAWB ? shipmentDetails.getCarrierDetails().getEtd() : null;
            AwbRoutingInfo routingInfo = new AwbRoutingInfo();
            routingInfo.setIsShipmentCreated(true);
            routingInfo.setOriginPortName(shipmentDetails.getCarrierDetails().getOriginPort());
            routingInfo.setDestinationPortName(shipmentDetails.getCarrierDetails().getDestinationPort());
            routingInfo.setByCarrier(shipmentDetails.getCarrierDetails().getShippingLine());
            routingInfo.setFlightNumber(shipmentDetails.getCarrierDetails().getFlightNumber());
            routingInfo.setFlightDate(flightDate);
            routingInfo.setEntityId(shipmentDetails.getId());
            routingInfo.setEntityType(request.getAwbType());
            if(awb.getAwbRoutingInfo() == null){
                awb.setAwbRoutingInfo(new ArrayList<>());
            }
            awb.getAwbRoutingInfo().add(routingInfo);
        }
    }
    private void updateAwbGoodsDescriptionInfoFromShipment(ShipmentDetails shipmentDetails, CreateAwbRequest request, Awb awb, HawbLockSettings hawbLockSettings, MawbLockSettings mawbLockSettings) {
        List<AwbGoodsDescriptionInfo> awbGoodsDescriptionInfoList = new ArrayList<>();
        if(awb.getAwbGoodsDescriptionInfo() != null && !awb.getAwbGoodsDescriptionInfo().isEmpty())
            awbGoodsDescriptionInfoList = awb.getAwbGoodsDescriptionInfo().stream().filter(good -> good.getIsShipmentCreated() != null && good.getIsShipmentCreated()).toList();
        if(awb.getAwbGoodsDescriptionInfo() == null || awb.getAwbGoodsDescriptionInfo().isEmpty() || awbGoodsDescriptionInfoList.isEmpty()){
            AwbGoodsDescriptionInfo awbGoodsDescriptionInfo = new AwbGoodsDescriptionInfo();
            Integer totalPacksCount = 0;
            awbGoodsDescriptionInfo.setIsShipmentCreated(true);
            awbGoodsDescriptionInfo.setEntityId(shipmentDetails.getId());
            awbGoodsDescriptionInfo.setEntityType(request.getAwbType());
            awbGoodsDescriptionInfo.setGrossWt(shipmentDetails.getWeight());
            awbGoodsDescriptionInfo.setGrossWtUnit(shipmentDetails.getWeightUnit());

            awbGoodsDescriptionInfo.setChargeableWt(shipmentDetails.getChargable() != null ?
                    AwbUtility.roundOffAirShipment((double) shipmentDetails.getChargable().doubleValue()) : null);
            awbGoodsDescriptionInfo.setGuid(UUID.randomUUID());
            if(awb.getAwbPackingInfo() != null) {
                for (var awbPacking: awb.getAwbPackingInfo() ) {
                    if(awbPacking.getGuid() != null && awbPacking.getAwbGoodsDescriptionInfoGuid() == null) {
                        awbPacking.setAwbGoodsDescriptionInfoGuid(awbGoodsDescriptionInfo.getGuid());
                        totalPacksCount += Integer.parseInt(awbPacking.getPacks());
                        if(awbGoodsDescriptionInfo.getAwbPackingInfo() == null){
                            awbGoodsDescriptionInfo.setAwbPackingInfo(new ArrayList<>());
                        }
                        awbGoodsDescriptionInfo.getAwbPackingInfo().add(awbPacking);
                    }
                }
            }
            awbGoodsDescriptionInfo.setPiecesNo(totalPacksCount);
            if(awb.getAwbGoodsDescriptionInfo() == null){
                awb.setAwbGoodsDescriptionInfo(new ArrayList<>());
            }
            awb.getAwbGoodsDescriptionInfo().add(awbGoodsDescriptionInfo);
        } else {
            AwbGoodsDescriptionInfo awbGoodsDescriptionInfo = awbGoodsDescriptionInfoList.get(0);
            Integer totalPacksCount = 0;
            if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getGrossWtLock()) ||
                    (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getGrossWtLock()))
                awbGoodsDescriptionInfo.setGrossWt(shipmentDetails.getWeight());
            if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getGrossWtUnitLock()) ||
                    (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getGrossWtUnitLock()))
                awbGoodsDescriptionInfo.setGrossWtUnit(shipmentDetails.getWeightUnit());

            if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getChargeableWtLock()) ||
                    (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getChargeableWtLock())) {
                awbGoodsDescriptionInfo.setChargeableWt(shipmentDetails.getChargable() != null ?
                        AwbUtility.roundOffAirShipment((double) shipmentDetails.getChargable().doubleValue()) : null);
            }
            if(awb.getAwbPackingInfo() != null) {
                for (var awbPacking: awb.getAwbPackingInfo() ) {
                    if(Objects.equals(awbPacking.getAwbGoodsDescriptionInfoGuid(), awbGoodsDescriptionInfo.getGuid()))
                        totalPacksCount += Integer.parseInt(awbPacking.getPacks());
                    if(awbPacking.getGuid() != null && awbPacking.getAwbGoodsDescriptionInfoGuid() == null) {
                        awbPacking.setAwbGoodsDescriptionInfoGuid(awbGoodsDescriptionInfo.getGuid());
                        totalPacksCount += Integer.parseInt(awbPacking.getPacks());
                        if(awbGoodsDescriptionInfo.getAwbPackingInfo() == null){
                            awbGoodsDescriptionInfo.setAwbPackingInfo(new ArrayList<>());
                        }
                        awbGoodsDescriptionInfo.getAwbPackingInfo().add(awbPacking);
                    }
                }
            }
            if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getPiecesNoLock()) ||
                    (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getPiecesNoLock()))
                awbGoodsDescriptionInfo.setPiecesNo(totalPacksCount);

        }
    }
    private void updateAwbCargoInfoFromShipment(ShipmentDetails shipmentDetails, CreateAwbRequest request, Awb awb, HawbLockSettings hawbLockSettings, MawbLockSettings mawbLockSettings) {
        AwbCargoInfo awbCargoInfo = awb.getAwbCargoInfo();
        if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getAccountingInfoLock()) ||
                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getAccountingInfoLock()))
            awbCargoInfo.setAccountingInfo(awbCargoInfo.getAccountingInfo() == null ? null : awbCargoInfo.getAccountingInfo().toUpperCase());
        if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getNtrQtyGoodsLock()) ||
                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getNtrQtyGoodsLock())) {
            awbCargoInfo.setNtrQtyGoods(shipmentDetails.getGoodsDescription());
            GenerateAwbPaymentInfoRequest generateAwbPaymentInfoRequest = new GenerateAwbPaymentInfoRequest();
            generateAwbPaymentInfoRequest.setAwbCargoInfo(awbCargoInfo);
            generateAwbPaymentInfoRequest.setAwbPackingInfo(awb.getAwbPackingInfo());
            generateAwbPaymentInfoRequest.setAwbGoodsDescriptionInfo(awb.getAwbGoodsDescriptionInfo());
            generateAwbPaymentInfoRequest.setIsFromShipment(false);
            generateAwbPaymentInfoRequest.setPackUpdate(false);
            awbCargoInfo.setNtrQtyGoods(getDims(generateAwbPaymentInfoRequest));
            awbCargoInfo.setNtrQtyGoods(awbCargoInfo.getNtrQtyGoods() == null ? null : awbCargoInfo.getNtrQtyGoods().toUpperCase());
        }

        if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getCustomsValueLock()) ||
                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getCustomsValueLock()))
            awbCargoInfo.setCustomsValue(new BigDecimal(0.0));
        if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getCurrencyLock()) ||
                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getCurrencyLock()))
            awbCargoInfo.setCurrency(userContext.getUser().getCompanyCurrency());
        if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getOtherInfoLock()) ||
                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getOtherInfoLock()))
            awbCargoInfo.setOtherInfo(awbCargoInfo.getOtherInfo() == null ? null : awbCargoInfo.getOtherInfo().toUpperCase());

        if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getShippingInformationLock()) ||
                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getShippingInformationLock())) {
            awbCargoInfo.setShippingInformation(awbCargoInfo.getShippingInformation() == null ? null : awbCargoInfo.getShippingInformation().toUpperCase());
            awbCargoInfo.setShippingInformationOther(awbCargoInfo.getShippingInformationOther() == null ? null : awbCargoInfo.getShippingInformationOther().toUpperCase());
        }
    }
    private void updateAwbOtherInfoFromShipment(ShipmentDetails shipmentDetails, CreateAwbRequest request, Awb awb, HawbLockSettings hawbLockSettings, MawbLockSettings mawbLockSettings) {
        AwbOtherInfo awbOtherInfo = awb.getAwbOtherInfo();
        if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getShipperLock()) ||
                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getShipperLock())) {
            var shipperName = StringUtility.convertToString(shipmentDetails.getConsigner() != null ? shipmentDetails.getConsigner().getOrgData().get(PartiesConstants.FULLNAME) : "");
            awbOtherInfo.setShipper(shipperName == null ? null : shipperName.toUpperCase());
        }
        if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getExecutedOnLock()) ||
                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getExecutedOnLock()))
            awbOtherInfo.setExecutedOn(jsonHelper.convertValue(DateTimeFormatter.ofPattern(Constants.YYYY_MM_DD_T_HH_MM_SS).format(LocalDateTime.now()), LocalDateTime.class));
    }

    @Override
    public ResponseEntity<IRunnerResponse> partialAutoUpdateMawb(CommonRequestModel commonRequestModel) throws RunnerException {
        String responseMsg;
        CreateAwbRequest request = (CreateAwbRequest) commonRequestModel.getData();
        if (request == null) {
            log.debug("Request is empty for MAWB Create for Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new ValidationException("Request can't be null");
        }

        if (request.getConsolidationId() == null) {
            log.error("Consolidation Id can't be null or empty in update MAWB Request");
            throw new ValidationException("Consolidation Id can't be null or empty in update MAWB Request");
        }

        List<Awb> awbs = awbDao.findByConsolidationId(request.getConsolidationId());
        if(awbs.isEmpty()){
            log.error("No Mawb exist for given Consolidation to update");
            throw new ValidationException("No Mawb exist for given Consolidation to update");
        }

        Awb awb = awbs.get(0);

        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();

        // fetch consolidation info
        ConsolidationDetails consolidationDetails = consolidationDetailsDao.findById(request.getConsolidationId()).get();
        if(shipmentSettingsDetails.getRestrictAWBEdit() != null && shipmentSettingsDetails.getRestrictAWBEdit()){
            ResetAwbRequest resetAwbRequest = ResetAwbRequest.builder()
                    .id(awb.getId())
                    .shipmentId(request.getShipmentId())
                    .consolidationId(request.getConsolidationId())
                    .awbType(request.getAwbType())
                    .resetType(AwbReset.ALL)
                    .build();
            return this.reset(CommonRequestModel.buildRequest(resetAwbRequest));
        } else if(shipmentSettingsDetails.getAutoUpdateShipmentAWB()) {
            updateMawbFromShipment(request, consolidationDetails, awb, shipmentSettingsDetails);
            awb = awbDao.save(awb);
        }

            // map mawb and hawb affter suuccessful save
        updateLinkHawbMawb(consolidationDetails, awb.getId());
        log.info("MAWB created successfully for Id {} with Request Id {}", awb.getId(), LoggerHelper.getRequestIdFromMDC());
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(awb));
    }

    private void updateLinkHawbMawb(ConsolidationDetails consolidationDetails, Long mawbId) {
        List<MawbHawbLink> mawbHawbLinks = mawbHawbLinkDao.findByMawbId(mawbId);
        Set<Long> linkedHawbIds = new HashSet<>();
        linkedHawbIds = mawbHawbLinks.stream().map(link -> link.getHawbId()).collect(Collectors.toSet());
        if(consolidationDetails.getShipmentsList() != null && !consolidationDetails.getShipmentsList().isEmpty()) {
            for (var consoleShipment : consolidationDetails.getShipmentsList()) {
                if (consoleShipment.getId() != null) {
                    Awb awb = awbDao.findByShipmentId(consoleShipment.getId()).stream().findFirst().get();

                    if (awb != null && !linkedHawbIds.contains(awb.getId())) {
                        MawbHawbLink mawbHawblink = new MawbHawbLink();
                        mawbHawblink.setHawbId(awb.getId());
                        mawbHawblink.setMawbId(mawbId);
                        mawbHawbLinkDao.save(mawbHawblink);
                    }
                }
            }
        }
    }

    private void updateMawbFromShipment(CreateAwbRequest request, ConsolidationDetails consolidationDetails, Awb awb, ShipmentSettingsDetails shipmentSettingsDetails) {

        MawbLockSettings mawbLockSettings = shipmentSettingsDetails.getMawbLockSettings();
        attachedShipmentDescriptions = new ArrayList<>();
        totalVolumetricWeightOfAwbPacks = BigDecimal.ZERO;
        if(awb.getAwbPackingInfo() == null){
            awb.setAwbPackingInfo(new ArrayList<>());
        }
        awb.setAwbPackingInfo(updateMawbPackingInfoFromShipment(consolidationDetails));
        updateMawbShipmentInfoFromShipment(consolidationDetails, request, awb, mawbLockSettings);
        generateMawbNotifyPartyinfo(consolidationDetails, request, awb, mawbLockSettings);
        updateMawbRoutingInfoFromShipment(consolidationDetails, request, awb, mawbLockSettings);
        updateMawbCargoInfoFromShipment(consolidationDetails, request, awb, mawbLockSettings);
        generateMawbOtherInfo(consolidationDetails, request, awb, mawbLockSettings);
    }
    private List<AwbPackingInfo> updateMawbPackingInfoFromShipment(ConsolidationDetails consolidationDetails) {
        List<AwbPackingInfo> awbPackingList = new ArrayList<>();
        List<AwbPackingInfo> hawbPacksLinkedToMawb = new ArrayList<>();

        if (consolidationDetails.getShipmentsList() != null && consolidationDetails.getShipmentsList().size() > 0) {
            for (var consoleShipment : consolidationDetails.getShipmentsList()) {
                if (!StringUtility.isEmpty(consoleShipment.getGoodsDescription())) {
                    attachedShipmentDescriptions.add(consoleShipment.getGoodsDescription());
                }

                var awbList = awbDao.findByShipmentId(consoleShipment.getId());
                if (awbList != null && !awbList.isEmpty()) {
                    var awb = awbList.stream().findFirst().get();
                    if (awb.getAwbPackingInfo() != null && awb.getAwbPackingInfo().size() > 0) {
                        for (var awbPack : awb.getAwbPackingInfo()) {
                            if (awbPack.getVolume() != null && !StringUtility.isEmpty(awbPack.getVolumeUnit()) &&
                                    awbPack.getVolumeUnit() == "M3") {
                                totalVolumetricWeightOfAwbPacks.add(awbPack.getVolume());
                            }
                            hawbPacksLinkedToMawb.add(awbPack);
                        }
                    }
                }
            }
            Double factor = Constants.FACTOR_VOL_WT;
            totalVolumetricWeightOfAwbPacks.multiply(new BigDecimal(factor));
        }
        return hawbPacksLinkedToMawb;
    }

    private void updateMawbShipmentInfoFromShipment(ConsolidationDetails consolidationDetails, CreateAwbRequest request, Awb awb, MawbLockSettings mawbLockSettings) {
        AwbShipmentInfo awbShipmentInfo = awb.getAwbShipmentInfo();
        var shipperName = StringUtility.convertToString(consolidationDetails.getSendingAgent().getOrgData().get(PartiesConstants.FULLNAME));
        awbShipmentInfo.setShipperName(shipperName == null ? shipperName : shipperName.toUpperCase());
        if(!mawbLockSettings.getFirstCarrierLock())
            awbShipmentInfo.setFirstCarrier(consolidationDetails.getCarrierDetails().getShippingLine());
        if(!mawbLockSettings.getShipperAddressLock()){
            var shipperAddress = AwbUtility.constructAddress(consolidationDetails.getSendingAgent() != null ? consolidationDetails.getSendingAgent().getAddressData() : null);
            awbShipmentInfo.setShipperAddress(shipperAddress == null ? shipperAddress : shipperAddress.toUpperCase());
        }
        var consigneeName = StringUtility.convertToString(consolidationDetails.getReceivingAgent() != null && consolidationDetails.getReceivingAgent().getOrgData() != null? consolidationDetails.getReceivingAgent().getOrgData().get(PartiesConstants.FULLNAME) : "");
        awbShipmentInfo.setConsigneeName(consigneeName == null ? consigneeName : consigneeName.toUpperCase());
        if(!mawbLockSettings.getConsigneeAddressLock()){
            var consigneeAddress = AwbUtility.constructAddress(consolidationDetails.getReceivingAgent() != null ? consolidationDetails.getReceivingAgent().getAddressData() : null);
            awbShipmentInfo.setConsigneeAddress(consigneeAddress == null ? consigneeAddress : consigneeAddress.toUpperCase());
        }
        awbShipmentInfo.setConsigneeReferenceNumber(consolidationDetails.getReceivingAgent() != null ? consolidationDetails.getReceivingAgent() .getId().toString() : null);
        // AwbUtility.getConsolidationForwarderDetails(uow, consolidationRow, awbShipmentInfo, awbOtherInfoRow, awbCargoInfo); TODO
        setAwbShipmentInfoUnLocationData(awbShipmentInfo, consolidationDetails.getCarrierDetails(), false, false);
    }

    private void generateMawbNotifyPartyinfo(ConsolidationDetails consolidationDetails, CreateAwbRequest request, Awb awb, MawbLockSettings mawbLockSettings) {
        Map<UUID, Parties> consolidationAddressMap = new HashMap<>();
        if (consolidationDetails.getConsolidationAddresses() != null &&
                consolidationDetails.getConsolidationAddresses().size() > 0) {
            for (var party : consolidationDetails.getConsolidationAddresses()) {
                if (party.getOrgData().get("Type") == "Notify Part 1" ||
                        party.getOrgData().get("Type") == "Notify Part 2" ||
                        party.getOrgData().get("Type") == "Notify Part 3") {
                    consolidationAddressMap.put(party.getGuid(), party);
                }
            }
        }
        List<AwbNotifyPartyInfo> deleteAwbPartyList = new ArrayList<>();
        if(awb.getAwbNotifyPartyInfo() != null && !awb.getAwbNotifyPartyInfo().isEmpty()){
            for (var awbParty: awb.getAwbNotifyPartyInfo()){
                if(awbParty.getIsShipmentCreated() != null && awbParty.getIsShipmentCreated()){
                    if(consolidationAddressMap.containsKey(awbParty.getGuid())){
                        Parties party = consolidationAddressMap.get(awbParty.getGuid());
                        if(!mawbLockSettings.getNotifyOrganizationLock()) {
                            var name = StringUtility.convertToString(party.getOrgData().get(PartiesConstants.FULLNAME));
                            awbParty.setName(name == null ? name : name.toUpperCase());
                        }
                        if(!mawbLockSettings.getNotifyOrganizationAddressLock())
                            awbParty.setAddress(AwbUtility.constructAddress(party.getAddressData()).toUpperCase());
                        consolidationAddressMap.remove(awbParty.getGuid());
                    } else {
                        deleteAwbPartyList.add(awbParty);
                    }
                }
            }
            awb.getAwbNotifyPartyInfo().removeAll(deleteAwbPartyList);
        }

        if(!consolidationAddressMap.isEmpty()){
            for (var party: consolidationAddressMap.values()){
                AwbNotifyPartyInfo notifyPartyInfo = new AwbNotifyPartyInfo();
                var name = StringUtility.convertToString(party.getOrgData().get(PartiesConstants.FULLNAME));
                notifyPartyInfo.setName(name == null ? name : name.toUpperCase());
                notifyPartyInfo.setAddress(AwbUtility.constructAddress(party.getAddressData()).toUpperCase());
                notifyPartyInfo.setEntityId(consolidationDetails.getId());
                notifyPartyInfo.setEntityType(request.getAwbType());
                notifyPartyInfo.setIsShipmentCreated(true);
                notifyPartyInfo.setGuid(party.getGuid());
                // notifyPartyInfo.setAddressId(shipmentNotifyParty.getAddressData()); // field missing: AddressId
                notifyPartyInfo.setNotifyOrgId(consolidationDetails.getId());
                if(awb.getAwbNotifyPartyInfo() == null){
                    awb.setAwbNotifyPartyInfo(new ArrayList<>());
                }
                awb.getAwbNotifyPartyInfo().add(notifyPartyInfo);
            }
        }

    }

    private void updateMawbRoutingInfoFromShipment(ConsolidationDetails consolidationDetails, CreateAwbRequest request, Awb awb, MawbLockSettings mawbLockSettings) {
        boolean createRouting = true;
        AwbRoutingInfo deleteParty = new AwbRoutingInfo();
        if(awb.getAwbRoutingInfo() != null && !awb.getAwbRoutingInfo().isEmpty()) {
            for (var awbRoute : awb.getAwbRoutingInfo()) {
                if (awbRoute.getIsShipmentCreated() != null && awbRoute.getIsShipmentCreated()) {
                    createRouting = false;
                    if (consolidationDetails.getCarrierDetails() != null &&
                            consolidationDetails.getCarrierDetails().getOriginPort() != null &&
                            consolidationDetails.getCarrierDetails().getDestinationPort() != null) {
                        if (!mawbLockSettings.getOriginPortLock()) {
                            awbRoute.setOriginPortName(consolidationDetails.getCarrierDetails().getOriginPort());
                        }
                        if (!mawbLockSettings.getDestinationPortLock()) {
                            awbRoute.setDestinationPortName(consolidationDetails.getCarrierDetails().getDestinationPort());
                        }
                        if (!mawbLockSettings.getByCarrierLock())
                            awbRoute.setByCarrier(consolidationDetails.getCarrierDetails().getShippingLine());
                        if (!mawbLockSettings.getFlightNumberLock())
                            awbRoute.setFlightNumber(consolidationDetails.getCarrierDetails().getFlightNumber());
                    } else {
                        deleteParty = awbRoute;
                    }
                }
            }
            awb.getAwbRoutingInfo().remove(deleteParty);
        }

        if (consolidationDetails.getCarrierDetails() != null &&
                consolidationDetails.getCarrierDetails().getOriginPort() != null &&
                consolidationDetails.getCarrierDetails().getDestinationPort() != null && createRouting
        ) {
            AwbRoutingInfo routingInfo = new AwbRoutingInfo();
            routingInfo.setIsShipmentCreated(true);
            routingInfo.setOriginPortName(consolidationDetails.getCarrierDetails().getOriginPort());
            routingInfo.setDestinationPortName(consolidationDetails.getCarrierDetails().getDestinationPort());
            routingInfo.setByCarrier(consolidationDetails.getCarrierDetails().getShippingLine());
            routingInfo.setFlightNumber(consolidationDetails.getCarrierDetails().getFlightNumber());
            routingInfo.setEntityId(consolidationDetails.getId());
            routingInfo.setEntityType(request.getAwbType());
            if(awb.getAwbRoutingInfo() == null){
                awb.setAwbRoutingInfo(new ArrayList<>());
            }
            awb.getAwbRoutingInfo().add(routingInfo);
        }
    }
    private void updateMawbCargoInfoFromShipment(ConsolidationDetails consolidationDetails, CreateAwbRequest request, Awb awb, MawbLockSettings mawbLockSettings) {
        AwbCargoInfo awbCargoInfo = awb.getAwbCargoInfo();
        String concatenatedGoodsDesc = ""; //TODO from consoleshipment mapping
        if (attachedShipmentDescriptions.size() > 0) {
            concatenatedGoodsDesc = String.join(",", attachedShipmentDescriptions);
        }
        if(!mawbLockSettings.getNtrQtyGoodsLock()) {
            String defaultTextForQuantAndGoods = Constants.DEFAULT_NATURE_AND_QUANTITY_GOODS_TEXT_MAWB;
            String newLine = "\r\n";
            awbCargoInfo.setNtrQtyGoods(concatenatedGoodsDesc);
            GenerateAwbPaymentInfoRequest generateAwbPaymentInfoRequest = new GenerateAwbPaymentInfoRequest();
            generateAwbPaymentInfoRequest.setAwbCargoInfo(awbCargoInfo);
            generateAwbPaymentInfoRequest.setAwbPackingInfo(awb.getAwbPackingInfo());
            generateAwbPaymentInfoRequest.setAwbGoodsDescriptionInfo(awb.getAwbGoodsDescriptionInfo());
            generateAwbPaymentInfoRequest.setIsFromShipment(false);
            generateAwbPaymentInfoRequest.setPackUpdate(false);
            awbCargoInfo.setNtrQtyGoods(defaultTextForQuantAndGoods + newLine+ getDims(generateAwbPaymentInfoRequest));
            awbCargoInfo.setNtrQtyGoods(awbCargoInfo.getNtrQtyGoods() == null ? null : awbCargoInfo.getNtrQtyGoods().toUpperCase());
        }

//        awbCargoInfo.setCarriageValue(shipmentDetails.getGoodsValue() != null ? shipmentDetails.getGoodsValue() : new BigDecimal(0.0)); // field missing
//        awbCargoInfo.setCarriageValue(shipmentDetails.getInsuranceValue() != null ? shipmentDetailsgetInsuranceValue() : new BigDecimal(0.0)); // field missing
        if(!mawbLockSettings.getCustomsValueLock())
            awbCargoInfo.setCustomsValue(new BigDecimal(0.0));
        if(!mawbLockSettings.getCurrencyLock())
            awbCargoInfo.setCurrency(userContext.getUser().getCompanyCurrency());
        awbCargoInfo.setHandlingInfo(getHandlingInfo(MasterDataType.MAWB_GENERATION, awb.getAwbPackingInfo(), consolidationDetails.getHazardous()));
        if(!mawbLockSettings.getAccountingInfoLock())
            awbCargoInfo.setAccountingInfo(awbCargoInfo.getAccountingInfo() == null ? null : awbCargoInfo.getAccountingInfo().toUpperCase());
        if(!mawbLockSettings.getOtherInfoLock())
            awbCargoInfo.setOtherInfo(awbCargoInfo.getOtherInfo() == null ? null : awbCargoInfo.getOtherInfo().toUpperCase());

        if(!mawbLockSettings.getShippingInformationLock()) {
            awbCargoInfo.setShippingInformation(awbCargoInfo.getShippingInformation() == null ? null : awbCargoInfo.getShippingInformation().toUpperCase());
            awbCargoInfo.setShippingInformationOther(awbCargoInfo.getShippingInformationOther() == null ? null : awbCargoInfo.getShippingInformationOther().toUpperCase());
        }

    }
    private void generateMawbOtherInfo(ConsolidationDetails consolidationDetails, CreateAwbRequest request, Awb awb, MawbLockSettings mawbLockSettings) {
        AwbOtherInfo awbOtherInfo = awb.getAwbOtherInfo();
        var shipperName = StringUtility.convertToString(consolidationDetails.getSendingAgent() != null ? consolidationDetails.getSendingAgent().getOrgData().get(PartiesConstants.FULLNAME) : "");
        awbOtherInfo.setShipper(shipperName == null ? null : shipperName.toUpperCase());
        if(!mawbLockSettings.getExecutedOnLock())
            awbOtherInfo.setExecutedOn(jsonHelper.convertValue(DateTimeFormatter.ofPattern(Constants.YYYY_MM_DD_T_HH_MM_SS).format(LocalDateTime.now()), LocalDateTime.class));

    }

    private void setTenantFieldsInAwbShipmentInfo(AwbShipmentInfo awbShipmentInfo, TenantModel tenantModel) {
        iataCode = tenantModel.AgentIATACode;
        awbShipmentInfo.setIataCode(iataCode);
        awbShipmentInfo.setAgentCASSCode(tenantModel.AgentCASSCode);
    }

    private String getHandlingInfo(MasterDataType masterDataType, List<AwbPackingInfo> awbPackingInfoList, Boolean dgFlag) {
        String res = null;
        List<Integer> itemTypeList = new ArrayList<>();
        itemTypeList.add(masterDataType.getId());
        List<Object> masterDataCriteria = Arrays.asList(
                Arrays.asList(MasterDataConstants.ITEM_TYPE),
                "in",
                Arrays.asList(itemTypeList)
        );
        CommonV1ListRequest masterDataRequest = CommonV1ListRequest.builder().skip(0).take(0).criteriaRequests(masterDataCriteria).build();
        V1DataResponse masterDataResponse = v1Service.fetchMasterData(masterDataRequest);
        if(masterDataResponse.getEntities() != null) {
            List<EntityTransferMasterLists> masterLists = jsonHelper.convertValueToList(masterDataResponse.entities, EntityTransferMasterLists.class);
            if(masterLists.size() > 0)
                res = masterLists.get(0).getItemDescription();
        }
        if(Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getAirDGFlag()) && Boolean.TRUE.equals(dgFlag)) {
            Integer packs = 0;
            if(awbPackingInfoList != null && !awbPackingInfoList.isEmpty()) {
                for (AwbPackingInfo awbPackingInfo: awbPackingInfoList) {
                    if(Boolean.TRUE.equals(awbPackingInfo.getHazardous()) && !IsStringNullOrEmpty(awbPackingInfo.getPacks())) {
                        packs = packs + Integer.parseInt(awbPackingInfo.getPacks());
                    }
                }
            }
            if(!IsStringNullOrEmpty(res))
                res = res + "\n";
            else
                res = "";
            res = res + "Dangerous Goods as per attached Shipper’s Declaration " + packs.toString();
        }
        return res;
    }

    @Override
    public ResponseEntity<IRunnerResponse> getAllMasterData(CommonRequestModel commonRequestModel, boolean isShipment) {
        String responseMsg;
        try {
            Long id = commonRequestModel.getId();
            List<Awb> optional = null;
            if(isShipment)
                optional = awbDao.findByShipmentId(id);
            else
                optional = awbDao.findByConsolidationId(id);
            if(optional == null || optional.isEmpty()) {
                log.debug("Shipment Details is null for Id {}", id);
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            Awb awb = optional.get(0);
            AwbResponse awbResponse = jsonHelper.convertValue(awb, AwbResponse.class);

            generateDefaultAwbInformation(awb,awbResponse);
            Map<String, Object> response = fetchAllMasterDataByKey(awb, awbResponse);
            return ResponseHelper.buildSuccessResponse(response);
        }
        catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_DATA_RETRIEVAL_FAILURE;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private Map<String, Object> fetchAllMasterDataByKey(Awb awb, AwbResponse awbResponse) {
        Map<String, Object> masterDataResponse = new HashMap<>();
        var masterListFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllMasterDataInSingleCall(awb, awbResponse, masterDataResponse)), executorService);
        var unLocationsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllUnlocationDataInSingleCall(awb, awbResponse, masterDataResponse)), executorService);
        var commodityTypesFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllCommodityTypesInSingleCallPacksList(awb, awbResponse, masterDataResponse)), executorService);
        CompletableFuture.allOf(masterListFuture, unLocationsFuture, commodityTypesFuture).join();
        return masterDataResponse;
    }

    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllMasterDataInSingleCall (Awb awb, AwbResponse awbResponse, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            AtomicInteger count = new AtomicInteger();
            List<MasterListRequest> listRequests = new ArrayList<>(masterDataUtils.createInBulkMasterListRequest(awbResponse, Awb.class, fieldNameKeyMap, Awb.class.getSimpleName() ));
            // Populate all the master data in inner objects
            if (!Objects.isNull(awbResponse.getAwbShipmentInfo()))
                listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(awbResponse.getAwbShipmentInfo(), AwbShipmentInfo.class, fieldNameKeyMap, AwbShipmentInfo.class.getSimpleName() ));
            if(!Objects.isNull(awbResponse.getAwbRoutingInfo()))
                awbResponse.getAwbRoutingInfo().forEach(r -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(r, AwbRoutingInfo.class, fieldNameKeyMap, AwbRoutingInfo.class.getSimpleName() + count.incrementAndGet())));
            if (!Objects.isNull(awbResponse.getDefaultAwbShipmentInfo()))
                listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(awbResponse.getDefaultAwbShipmentInfo(), AwbShipmentInfo.class, fieldNameKeyMap, AwbShipmentInfo.class.getSimpleName() ));
            if(!Objects.isNull(awbResponse.getDefaultAwbRoutingInfo()))
                awbResponse.getDefaultAwbRoutingInfo().forEach(r -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(r, AwbRoutingInfo.class, fieldNameKeyMap, AwbRoutingInfo.class.getSimpleName() + count.incrementAndGet())));
            if(!Objects.isNull(awbResponse.getAwbPackingInfo()))
                awbResponse.getAwbPackingInfo().forEach(r -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(r, AwbPackingInfo.class, fieldNameKeyMap, AwbPackingInfo.class.getSimpleName() + count.incrementAndGet() )));
            if(!Objects.isNull(awbResponse.getAwbGoodsDescriptionInfo()))
                awbResponse.getAwbGoodsDescriptionInfo().forEach(r -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(r, AwbGoodsDescriptionInfo.class, fieldNameKeyMap, AwbGoodsDescriptionInfo.class.getSimpleName() + count.incrementAndGet())));

            MasterListRequestV2 masterListRequestV2 = new MasterListRequestV2();
            masterListRequestV2.setMasterListRequests(listRequests);
            masterListRequestV2.setIncludeCols(Arrays.asList(MasterDataConstants.ITEM_TYPE, MasterDataConstants.ITEM_VALUE, "ItemDescription", "ValuenDesc", "Cascade"));

            Map<String, EntityTransferMasterLists> keyMasterDataMap = masterDataUtils.fetchInBulkMasterList(masterListRequestV2);
            masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.MASTER_LIST);

            if(masterDataResponse == null) {
                awbResponse.setMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(Awb.class.getSimpleName()), CacheConstants.MASTER_LIST));
                if (!Objects.isNull(awbResponse.getAwbShipmentInfo()))
                    awbResponse.getAwbShipmentInfo().setMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(AwbShipmentInfo.class.getSimpleName()), CacheConstants.MASTER_LIST) );

            }
            else {
                masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.MASTER_LIST, masterDataResponse);
            }

            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(keyMasterDataMap));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occured in CompletableFuture: addAllMasterDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), AwbService.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }

    }

    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllUnlocationDataInSingleCall (Awb awb, AwbResponse awbResponse, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            AtomicInteger count = new AtomicInteger();
            List<String> locationCodes = new ArrayList<>();
            // Populate all the unlocation data in inner objects
            if (!Objects.isNull(awbResponse.getAwbShipmentInfo()))
                locationCodes.addAll((masterDataUtils.createInBulkUnLocationsRequest(awbResponse.getAwbShipmentInfo(), AwbShipmentInfo.class, fieldNameKeyMap, AwbShipmentInfo.class.getSimpleName() )));
            if (!Objects.isNull(awbResponse.getAwbOtherInfo()))
                locationCodes.addAll((masterDataUtils.createInBulkUnLocationsRequest(awbResponse.getAwbOtherInfo(), AwbOtherInfo.class, fieldNameKeyMap, AwbOtherInfo.class.getSimpleName() )));
            if(!Objects.isNull(awbResponse.getAwbRoutingInfo()))
                awbResponse.getAwbRoutingInfo().forEach(r -> locationCodes.addAll(masterDataUtils.createInBulkUnLocationsRequest(r, AwbRoutingInfo.class, fieldNameKeyMap, AwbRoutingInfo.class.getSimpleName() + (count.incrementAndGet()))));
            if (!Objects.isNull(awbResponse.getDefaultAwbShipmentInfo()))
                locationCodes.addAll((masterDataUtils.createInBulkUnLocationsRequest(awbResponse.getDefaultAwbShipmentInfo(), AwbShipmentInfo.class, fieldNameKeyMap, AwbShipmentInfo.class.getSimpleName() + Constants.class )));
            if(!Objects.isNull(awbResponse.getDefaultAwbRoutingInfo()))
                awbResponse.getDefaultAwbRoutingInfo().forEach(r -> locationCodes.addAll(masterDataUtils.createInBulkUnLocationsRequest(r, AwbRoutingInfo.class, fieldNameKeyMap, AwbRoutingInfo.class.getSimpleName() + (count.incrementAndGet()))));
            if(!Objects.isNull(awbResponse.getAwbPackingInfo()))
                awbResponse.getAwbPackingInfo().forEach(r -> locationCodes.addAll(masterDataUtils.createInBulkUnLocationsRequest(r, AwbPackingInfo.class, fieldNameKeyMap, AwbPackingInfo.class.getSimpleName() + (count.incrementAndGet()))));
            if(!Objects.isNull(awbResponse.getAwbGoodsDescriptionInfo()))
                awbResponse.getAwbGoodsDescriptionInfo().forEach(r -> locationCodes.addAll(masterDataUtils.createInBulkUnLocationsRequest(r, AwbGoodsDescriptionInfo.class, fieldNameKeyMap, AwbGoodsDescriptionInfo.class.getSimpleName() + (count.incrementAndGet()))));

            Map<String, EntityTransferUnLocations> keyMasterDataMap = masterDataUtils.fetchInBulkUnlocations(locationCodes, EntityTransferConstants.LOCATION_SERVICE_GUID);
            masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.UNLOCATIONS);

            if(masterDataResponse == null) {
                if (!Objects.isNull(awbResponse.getAwbShipmentInfo()))
                    awbResponse.getAwbShipmentInfo().setUnlocationData(masterDataUtils.setMasterData(fieldNameKeyMap.get(AwbShipmentInfo.class.getSimpleName()), CacheConstants.UNLOCATIONS_AWB));
            }
            else {
                masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.UNLOCATIONS_AWB, masterDataResponse);
            }

            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(keyMasterDataMap));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occured in CompletableFuture: addAllUnlocationDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), AwbService.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }
    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllCommodityTypesInSingleCallPacksList(Awb awb, AwbResponse awbResponse, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> commodityTypes = new HashSet<>();
            AtomicInteger count = new AtomicInteger();
            if(!Objects.isNull(awbResponse.getAwbPackingInfo()))
                awbResponse.getAwbPackingInfo().forEach(r -> commodityTypes.addAll(masterDataUtils.createInBulkCommodityTypeRequest(r, AwbPackingInfo.class, fieldNameKeyMap, AwbRoutingInfo.class.getSimpleName() + count.incrementAndGet() )));

            Map<String, EntityTransferCommodityType> v1Data = masterDataUtils.fetchInBulkCommodityTypes(commodityTypes.stream().toList());
            masterDataUtils.pushToCache(v1Data, CacheConstants.COMMODITY);

            if(masterDataResponse == null) {
                if (!Objects.isNull(awbResponse.getAwbShipmentInfo()))
                    awbResponse.getAwbShipmentInfo().setCommodityMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(AwbShipmentInfo.class.getSimpleName()), CacheConstants.COMMODITY));
            }
            else {
                masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.COMMODITY, masterDataResponse);
            }

            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(v1Data));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occured in CompletableFuture: addAllCommodityTypesInSingleCallPacksList in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), AwbService.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> generateAwbPaymentInfo(CommonRequestModel commonRequestModel) throws RunnerException {

        GenerateAwbPaymentInfoRequest req = (GenerateAwbPaymentInfoRequest) commonRequestModel.getData();

        /*
            1. Calculate good description based on packs
            2. Calculate other infos based on good description
         */
        updateGoodsDescriptionInfoFromPacks(req);
        updateOtherChargesFromGoodsDescription(req);

        double totalAmount = 0.00;
        if(req.getAwbGoodsDescriptionInfo() != null) {
            for(var goods : req.getAwbGoodsDescriptionInfo()) {
                totalAmount += goods.getTotalAmount() != null ? goods.getTotalAmount().doubleValue() : 0;
            }
        }

        double agentOtherCharges = calculateOtherCharges(req, ChargesDue.AGENT);
        double carrierOtherCharges = calculateOtherCharges(req, ChargesDue.CARRIER);

        AwbPaymentInfo paymentInfo = AwbPaymentInfo.builder()
                .weightCharges(new BigDecimal(totalAmount))
                .dueAgentCharges(agentOtherCharges != 0 ? new BigDecimal(agentOtherCharges) : null)
                .dueCarrierCharges(carrierOtherCharges != 0 ? new BigDecimal(carrierOtherCharges) : null)
                .build();

        if(req.getAwbPaymentInfo() != null ){
            paymentInfo = req.getAwbPaymentInfo();
            paymentInfo.setWeightCharges(new BigDecimal(totalAmount));
            paymentInfo.setDueAgentCharges(agentOtherCharges != 0 ? new BigDecimal(agentOtherCharges) : null);
            paymentInfo.setDueCarrierCharges(carrierOtherCharges != 0 ? new BigDecimal(carrierOtherCharges) : null);
        }

        if(req.getChargeDetails() != null) {
            double totalPrepaid = 0.00;
            double totalCollect = 0.00;

            double prepaidWeightCharge = 0.00;
            double collectWeightCharge = 0.00;
            double prepaidValuationCharge = 0.00;
            double collectValuationCharge = 0.00;
            double prepaidTax = 0.00;
            double collectTax = 0.00;
            double prepaidDueAgentCharges = 0.00;
            double collectDueAgentCharges = 0.00;
            double prepaidDueCarrierCharges = 0.00;
            double collectDueCarrierCharges = 0.00;

            if(!Objects.isNull(req.getChargeDetails().getIdentifier1()) && req.getChargeDetails().getIdentifier1().equals(Constants.TRUE)) {
                // Prepaid WeighCharges
                prepaidWeightCharge = totalAmount;
                prepaidValuationCharge = getDoubleValue(req.getAwbPaymentInfo().getValuationCharge());
                prepaidTax = getDoubleValue(req.getAwbPaymentInfo().getTax());
            } else {
                prepaidWeightCharge = 0.00;
                prepaidValuationCharge = 0.00;
                prepaidTax = 0.00;
            }

            if(!Objects.isNull(req.getChargeDetails().getIdentifier2()) && req.getChargeDetails().getIdentifier2().equals(Constants.TRUE)) {
                // CollectWeightCharges
                collectWeightCharge = totalAmount;
                collectValuationCharge = getDoubleValue(req.getAwbPaymentInfo().getValuationCharge());
                collectTax = getDoubleValue(req.getAwbPaymentInfo().getTax());
            } else {
                collectWeightCharge = 0.00;
                collectValuationCharge = 0.00;
                collectTax = 0.00;
            }

            if(!Objects.isNull(req.getChargeDetails().getIdentifier3()) && req.getChargeDetails().getIdentifier3().equals(Constants.TRUE)) {
                // PrepaidDueAgentCharges
                // PrepaidDueCarrierCharges
                prepaidDueAgentCharges = agentOtherCharges;
                prepaidDueCarrierCharges = carrierOtherCharges;
            } else{
                prepaidDueAgentCharges = 0.00;
                prepaidDueCarrierCharges = 0.00;
            }

            if(!Objects.isNull(req.getChargeDetails().getIdentifier4()) && req.getChargeDetails().getIdentifier4().equals(Constants.TRUE)) {
                // CollectDueAgentCharges
                // CollectDueCarrierCharges
                collectDueAgentCharges = agentOtherCharges;
                collectDueCarrierCharges = carrierOtherCharges;
            } else {
                collectDueAgentCharges = 0.00;
                collectDueCarrierCharges = 0.00;
            }

            totalPrepaid = prepaidWeightCharge + prepaidValuationCharge + prepaidTax + prepaidDueAgentCharges + prepaidDueCarrierCharges;
            totalCollect = collectWeightCharge + collectValuationCharge + collectTax + collectDueAgentCharges + collectDueCarrierCharges;

            paymentInfo.setTotalCollect(convertToBigDecimal(totalCollect));
            paymentInfo.setTotalPrepaid(convertToBigDecimal(totalPrepaid));
        }


        return ResponseHelper.buildSuccessResponse(AwbCalculationResponse.builder()
                .awbPackingInfo(req.getAwbPackingInfo()).awbGoodsDescriptionInfo(req.getAwbGoodsDescriptionInfo())
                .awbOtherChargesInfo(req.getAwbOtherChargesInfo()).awbPaymentInfo(paymentInfo).build());
    }

    private double calculateOtherCharges(GenerateAwbPaymentInfoRequest req, ChargesDue chargesDue) {
        double sum = 0.00;
        if(req.getAwbOtherChargesInfo() != null) {
            for(var otherCharges : req.getAwbOtherChargesInfo()) {
                if(otherCharges.getChargeDue() != null && chargesDue.equals(ChargesDue.getById(otherCharges.getChargeDue())))
                    sum += otherCharges.getAmount() != null ? otherCharges.getAmount().doubleValue() : 0;
            }
        }
        return sum;
    }

    public ResponseEntity<IRunnerResponse> retrieveByAwbByMawb(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request.getId() == null) {
                log.error("Request Id is null for MAWB retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new RunnerException("Request id can't be null");
            }
            long id = request.getId();
            List<Awb> awb = getLinkedAwbFromMawb(id);
            if (awb == null) {
                log.debug(AwbConstants.AWB_RETRIEVE_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("AWB fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());

            if(request.getIncludeColumns()==null||request.getIncludeColumns().size()==0)
                return ResponseHelper.buildSuccessResponse(jsonHelper.convertValueToList(awb, AwbResponse.class));
            else {
                List<Object> data = new ArrayList<>();
                for(Awb awb1 : awb) {
                    data.add(partialFetchUtils.fetchPartialListData(jsonHelper.convertValue(awb, AwbResponse.class), request.getIncludeColumns()));
                }
                return ResponseHelper.buildSuccessResponse(data);
            }

        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private BigDecimal updateGoodsDescriptionInfoFromPacks(GenerateAwbPaymentInfoRequest request) throws RunnerException {
        BigDecimal totalVolumeticWeight = BigDecimal.ZERO;
        List<AwbPackingInfo> packsInfo = request.getAwbPackingInfo();
        List<AwbGoodsDescriptionInfo> goodsDescriptionInfos = request.getAwbGoodsDescriptionInfo();
        var tenantSettingsList = shipmentSettingsDao.getSettingsByTenantIds(Arrays.asList(UserContext.getUser().TenantId));
        ShipmentSettingsDetails tenantSettings = null;
        if (tenantSettingsList != null && tenantSettingsList.size() >= 1) {
            tenantSettings = tenantSettingsList.get(0);
        }

        if (!Objects.isNull(goodsDescriptionInfos)) {
            Map<UUID, List<AwbPackingInfo>> guidBasedAwbPackingList = new HashMap<>();
            if (!Objects.isNull(packsInfo)) {
                if(!Objects.isNull(request.getIsFromShipment()) && Boolean.TRUE.equals(request.getIsFromShipment()) && Objects.equals(goodsDescriptionInfos.get(0).getEntityType(), Constants.DMAWB)){
                    if(goodsDescriptionInfos.size() == 1){
                        request.setPackUpdate(true);
                        guidBasedAwbPackingList.put(goodsDescriptionInfos.get(0).getGuid(), packsInfo);
                    }
                }
                else if (!Objects.isNull(request.getIsFromShipment()) && Boolean.TRUE.equals(request.getIsFromShipment()))
                    guidBasedAwbPackingList = packsInfo.stream().filter(c -> !Objects.isNull(c.getAwbGoodsDescriptionInfoGuid()))
                            .collect(Collectors.groupingBy(AwbPackingInfo::getAwbGoodsDescriptionInfoGuid));
                else
                    guidBasedAwbPackingList.put(goodsDescriptionInfos.get(0).getGuid(), packsInfo);
            }

            for (int i = 0; i < goodsDescriptionInfos.size(); i++) {
                var allPacks = guidBasedAwbPackingList.get(goodsDescriptionInfos.get(i).getGuid());
                Pair<BigDecimal, AwbGoodsDescriptionInfo> pair = calculateGoodsDescription(goodsDescriptionInfos.get(i), allPacks, tenantSettings, new HashMap<>(), request.isPackUpdate());
                totalVolumeticWeight = totalVolumeticWeight.add(pair.getLeft());
            }
        }
        return totalVolumeticWeight;
    }

    private void updateOtherChargesFromGoodsDescription(GenerateAwbPaymentInfoRequest request) {
        List<AwbGoodsDescriptionInfo> goodsDescriptionInfos = request.getAwbGoodsDescriptionInfo();
        List<AwbOtherChargesInfo> otherChargesInfos = request.getAwbOtherChargesInfo();
        var chargeableWeight = new BigDecimal(0);
        var grossWeight = new BigDecimal(0);

        if (!Objects.isNull(goodsDescriptionInfos)) {
            chargeableWeight = calculateChargealeWeight(goodsDescriptionInfos);
            grossWeight = calculateGrossWeight(goodsDescriptionInfos);
        }

        if (!Objects.isNull(otherChargesInfos)) {

            for (var _otherCharge : otherChargesInfos) {
                if (_otherCharge.getChargeBasis() == 1)
                    _otherCharge.setAmount(_otherCharge.getRate());
                else if (_otherCharge.getChargeBasis() == 2)
                    _otherCharge.setAmount(_otherCharge.getRate().multiply(chargeableWeight));
                else if (_otherCharge.getChargeBasis() == 3)
                    _otherCharge.setAmount(_otherCharge.getRate().multiply(grossWeight));
            }

        }

    }

    private BigDecimal calculateChargealeWeight(List<AwbGoodsDescriptionInfo> goodsDescriptionInfos) {
        BigDecimal chargeableWeight = new BigDecimal(0);
        for (var goods : goodsDescriptionInfos) {
           if (!Objects.isNull(goods.getChargeableWt()))
               chargeableWeight = chargeableWeight.add(goods.getChargeableWt());
        }

        return chargeableWeight;
    }

    private BigDecimal calculateGrossWeight(List<AwbGoodsDescriptionInfo> goodsDescriptionInfos) {
        BigDecimal grossWeight = new BigDecimal(0);
        String grossWtUnit = null;
        var flag = true;
        for (var goods : goodsDescriptionInfos) {
            if (Objects.isNull(grossWtUnit)) {
                grossWtUnit = goods.getGrossWtUnit();
                if (!Objects.isNull(goods.getGrossWt())) {
                    grossWeight = grossWeight.add(goods.getGrossWt());
                }
            } else if (flag && grossWtUnit.equals(goods.getGrossWtUnit())) {
                if (!Objects.isNull(goods.getGrossWt())) {
                    grossWeight = grossWeight.add(goods.getGrossWt());
                }
            } else {
                grossWeight = new BigDecimal(0);
                flag = false;
            }
        }

        return grossWeight;
    }

    public BigDecimal convertToBigDecimal(Number number) {
        if (number instanceof Integer
                || number instanceof Long
                || number instanceof Short
                || number instanceof Byte) {
            return BigDecimal.valueOf(number.longValue());
        }
        return BigDecimal.valueOf(number.doubleValue());
    }


    @Override
    public ResponseEntity<IRunnerResponse> generateUpdatedNatureAndQuantGoodsField(CommonRequestModel commonRequestModel) throws RunnerException {
        GenerateAwbPaymentInfoRequest request = (GenerateAwbPaymentInfoRequest) commonRequestModel.getData();
        return ResponseHelper.buildSuccessResponse(getDims(request));
    }

    private String getDims(GenerateAwbPaymentInfoRequest request) {
        try {
            String natureAndQuantGoodsValue = request.getAwbCargoInfo() == null || request.getAwbCargoInfo().getNtrQtyGoods() == null ? null : request.getAwbCargoInfo().getNtrQtyGoods();
            String packsDescriptionValue = "";
            String dimensionText = Constants.DEFAULT_DIMN_TEXT;
            Set<String> uniqueDimension = new HashSet<>();
            String newLine = "\r\n";

            if (request.getAwbPackingInfo() != null && request.getAwbPackingInfo().size() > 0) {
                if (StringUtility.isNotEmpty(natureAndQuantGoodsValue)) {
                    natureAndQuantGoodsValue += newLine;
                }
                int counter = 0;
                for (AwbPackingInfo packings : request.getAwbPackingInfo()) {
                    String pcs = " ";
                    String len = " ";
                    String width = " ";
                    String height = " ";
                    String equals = Constants.EQ;
                    String cross = Constants.CROSS;

                    if (packings.getPacks() != null) {
                        pcs = packings.getPacks() + equals;
                    } else {
                        pcs += equals;
                    }

                    if (packings.getLength() != null) {
                        len = packings.getLength().toString() + cross;
                    } else {
                        len += cross;
                    }

                    if (packings.getWidth() != null) {
                        width = packings.getWidth().toString() + cross;
                    } else {
                        width += cross;
                    }

                    if (packings.getHeight() != null) {
                        height = packings.getHeight().toString();
                    }
                    if (StringUtility.isNotEmpty(packings.getLengthUnit()) && StringUtility.isNotEmpty(packings.getWidthUnit()) && StringUtility.isNotEmpty(packings.getHeightUnit())) {
                        uniqueDimension.add(packings.getLengthUnit());
                        uniqueDimension.add(packings.getWidthUnit());
                        uniqueDimension.add(packings.getHeightUnit());
                    }

                    packsDescriptionValue += pcs + len + width + height + ",";
                    if (counter == request.getAwbPackingInfo().size() - 1) {
                        packsDescriptionValue = packsDescriptionValue.substring(0, packsDescriptionValue.length() - 1);
                    }

                    counter++;
                    if (counter % 2 == 0) {
                        packsDescriptionValue += newLine;
                    }
                }

                if (uniqueDimension.size() == 1) {
                    String dimentionUnit = new ArrayList<>(uniqueDimension).get(0);
                    if (dimentionUnit != null) {
                        if (dimentionUnit.equalsIgnoreCase(Constants.CM)) {
                            dimentionUnit = Constants.CMS;
                        } else if (dimentionUnit.equalsIgnoreCase(Constants.IN)) {
                            dimentionUnit = Constants.INCH;
                        } else if (dimentionUnit.equalsIgnoreCase(Constants.M)) {
                            dimentionUnit = Constants.MTR;
                        } else if (dimentionUnit.equalsIgnoreCase(Constants.FT)) {
                            dimentionUnit = Constants.FEET;
                        } else {
                            dimentionUnit = "";
                        }
                    }
                    dimensionText += dimentionUnit + newLine;
                } else {
                    dimensionText += newLine;
                }

                if (counter % 2 != 0) {
                    packsDescriptionValue += newLine;
                }

                BigDecimal totalVW = updateGoodsDescriptionInfoFromPacks(request);
                BigDecimal totalVWt = totalVW.setScale(2, RoundingMode.HALF_UP);

                packsDescriptionValue += "Total Volumetric Weight " + totalVWt.toString() + " ";

                ShipmentSettingsDetails tenantSettingsList = commonUtils.getShipmentSettingFromContext();

                if (tenantSettingsList != null && tenantSettingsList.getWeightChargeableUnit().equalsIgnoreCase(Constants.WEIGHT_UNIT_KG)) {
                    packsDescriptionValue += Constants.KGS;
                }

            } else {
                return natureAndQuantGoodsValue;
            }

            StringBuilder responseBuilder = new StringBuilder(StringUtility.isEmpty(natureAndQuantGoodsValue) ? StringUtility.getEmptyString() : natureAndQuantGoodsValue);
            responseBuilder.append(dimensionText).append(packsDescriptionValue);
            return responseBuilder.toString();
        } catch (RunnerException ex){
            return "";
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> getChargeTypeMasterData(CommonGetRequest commonGetRequest) throws RunnerException {
        Long chargeTypeId = commonGetRequest.getId();
        if(chargeTypeId == null)
            throw new RunnerException("Please provide a valid Id");

        V1RetrieveRequest retrieveRequest = V1RetrieveRequest.builder().EntityId(String.valueOf(chargeTypeId)).build();
        V1RetrieveResponse v1DataResponse = v1Service.retrieveChargeCodeData(retrieveRequest);

        var chargeType = jsonHelper.convertValue(v1DataResponse.getEntity(), EntityTransferChargeType.class);
        var res = new AwbChargeTypeMasterDataResponse();
        for(var i : chargeType.getChargeTypeIntegrations()) {
            if(i.getIntegrationType().equals(ChargeTypeCode.IATA_Charge_Code)) {
                res.setIataDescription(i.getIntegrationCode());
            }
            if(i.getIntegrationType().equals(ChargeTypeCode.Due_To_Party)) {
                res.setChargeDue(i.getChargeDue());
            }
        }
        return ResponseHelper.buildSuccessResponse(res);
    }

    private String getFormattedAddress(EntityTransferOrganizations organization) {
        AwbAddressParam addressParam = AwbAddressParam.builder()
                .address1(organization.getAddress1())
                .address2(organization.getAddress2())
                .city(organization.getCity())
                .state(organization.getState())
                .country(organization.getCountry())
                .pinCode(organization.getZipPostCode())
                .contactNumber(organization.getPhone())
                .build();
        return AwbUtility.getFormattedAddress(addressParam);
    }

    private double getDoubleValue(BigDecimal number) {
        if(number == null)
            return 0.0;
        return number.doubleValue();
    }

    private AwbShipmentInfoResponse generateDefaultAwbInformation(Awb awb, AwbResponse awbResponse) {
        String awbType = awb.getAwbShipmentInfo().getEntityType();
        // Default objects for UI
        AwbShipmentInfoResponse defaultAwbShipmentInfo = null;
        List<AwbNotifyPartyInfo> defaultNotifyPartyInfo = null;
        List<AwbRoutingInfoResponse> defaultRoutingInfo = null;
        CreateAwbRequest createAwbRequest = CreateAwbRequest.builder()
                .ConsolidationId(awb.getConsolidationId())
                .ShipmentId(awb.getShipmentId())
                .AwbType(awb.getAwbShipmentInfo().getEntityType())
                .build();
        try {
            if(awbType.equalsIgnoreCase(MAWB)) {
                ConsolidationDetails consol = consolidationDetailsDao.findById(awb.getConsolidationId()).get();
                defaultAwbShipmentInfo = jsonHelper.convertValue(generateMawbShipmentInfo(consol, createAwbRequest, new AwbCargoInfo()), AwbShipmentInfoResponse.class);
                defaultRoutingInfo = jsonHelper.convertValueToList(generateMawbRoutingInfo(consol, createAwbRequest), AwbRoutingInfoResponse.class);
                defaultNotifyPartyInfo = generateMawbNotifyPartyinfo(consol, createAwbRequest);
            } else {
                ShipmentDetails shipment = shipmentDao.findById(awb.getShipmentId()).get();
                defaultAwbShipmentInfo = jsonHelper.convertValue(generateAwbShipmentInfo(shipment, createAwbRequest, new AwbCargoInfo()), AwbShipmentInfoResponse.class);
                defaultRoutingInfo = jsonHelper.convertValueToList(generateAwbRoutingInfo(shipment, createAwbRequest), AwbRoutingInfoResponse.class);
                defaultNotifyPartyInfo = generateAwbNotifyPartyinfo(shipment, createAwbRequest);
            }
            awbResponse.setDefaultAwbShipmentInfo(defaultAwbShipmentInfo);
            awbResponse.setDefaultAwbNotifyPartyInfo(defaultNotifyPartyInfo);
            awbResponse.setDefaultAwbRoutingInfo(defaultRoutingInfo);
        } catch (Exception e) {
            log.error("Error while creating default awbShipmentInfo object for awb having id {} with error \n {}", awb.getId(), e.getMessage());
        }

        return defaultAwbShipmentInfo;
    }

    private void setAwbShipmentInfoUnLocationData(AwbShipmentInfo awbShipmentInfo, CarrierDetails carrierDetails, boolean blockOrigin, boolean blockDestination) {

        List<String> locationReferenceGuids = new ArrayList<>();
        locationReferenceGuids.add(carrierDetails.getOriginPort());
        locationReferenceGuids.add(carrierDetails.getDestinationPort());

        List<Object> criteria = Arrays.asList(
                Arrays.asList(EntityTransferConstants.LOCATION_SERVICE_GUID),
                "in",
                List.of(locationReferenceGuids)
        );
        CommonV1ListRequest commonV1ListRequest = CommonV1ListRequest.builder().skip(0).take(0).criteriaRequests(criteria).build();
        V1DataResponse v1DataResponse = v1Service.fetchUnlocation(commonV1ListRequest);

        List<EntityTransferUnLocations> locationDataList = jsonHelper.convertValueToList(v1DataResponse.entities, EntityTransferUnLocations.class);

        var locMap = locationDataList.stream().collect(Collectors.toMap(EntityTransferUnLocations::getLocationsReferenceGUID, EntityTransferUnLocations::getName, (oldValue, newValue) -> oldValue));
        if(!blockOrigin) {
           awbShipmentInfo.setOriginAirport(locMap.get(carrierDetails.getOriginPort()));
        }
        if(!blockDestination) {
            awbShipmentInfo.setDestinationAirport(locMap.get(carrierDetails.getDestinationPort()));
        }
    }

    private void getAwbOtherInfoMasterData(AwbOtherInfo awbOtherInfo, String awbType) {
        MasterDataType masterDataType;
        if(awbType.equalsIgnoreCase("HAWB")) {
            masterDataType = MasterDataType.HAWB_CARRIER_AGENT;
        } else {
            masterDataType = MasterDataType.MAWB_CARRIER_AGENT;
        }
        List<Object> criteria = Arrays.asList(
                List.of(MasterDataConstants.ITEM_TYPE),
                "=",
                masterDataType.getId()
        );
        CommonV1ListRequest listRequest = CommonV1ListRequest.builder().skip(0).take(0).criteriaRequests(criteria).build();
        V1DataResponse v1DataResponse = v1Service.fetchMasterData(listRequest);
        List<EntityTransferMasterLists> entityTransferMasterList = jsonHelper.convertValueToList(v1DataResponse.entities, EntityTransferMasterLists.class);
        if(entityTransferMasterList != null && entityTransferMasterList.size() > 0) {
            awbOtherInfo.setCarrier(entityTransferMasterList.get(0).getItemValue());
        }

    }

    private String setUnLocationDataWithDiarcties(String name) {
        List<String> diarcties = new ArrayList<>();
        diarcties.add(name);

        List<Object> criteria = Arrays.asList(
                Arrays.asList(EntityTransferConstants.NAME_WO_DIACRITICS),
                Operators.IN.getValue(),
                List.of(diarcties)
        );
        CommonV1ListRequest commonV1ListRequest = CommonV1ListRequest.builder().skip(0).take(0).criteriaRequests(criteria).build();

        V1DataResponse v1DataResponse;
        try {
            v1DataResponse = v1Service.fetchUnlocation(commonV1ListRequest);
        } catch (Exception e) {
            return StringUtility.getEmptyString();
        }

        List<EntityTransferUnLocations> locationDataList = jsonHelper.convertValueToList(v1DataResponse.entities, EntityTransferUnLocations.class);
        var locMap = locationDataList.stream().collect(Collectors.toMap(EntityTransferUnLocations::getNameWoDiacritics, EntityTransferUnLocations::getLocationsReferenceGUID,
                (locationguid1, locationguid2) -> {
            return locationguid1;
        }));
        return locMap.get(name);
    }

    private void populateIssuingAgent(AwbShipmentInfo awbShipmentInfo, TenantModel tenantModel, AwbCargoInfo awbCargoInfo) throws RunnerException {
        if(tenantModel.DefaultOrgId != null) {
            // Fetch Organization Data for defaultOrgId
            try {
                CommonV1ListRequest orgRequest = new CommonV1ListRequest();
                List<Object> orgField = new ArrayList<>(List.of("Id"));
                String operator = "=";
                List<Object> orgCriteria = new ArrayList<>(List.of(orgField, operator, tenantModel.DefaultOrgId));
                orgRequest.setCriteriaRequests(orgCriteria);
                V1DataResponse orgResponse = v1Service.fetchOrganization(orgRequest);
                List<EntityTransferOrganizations> orgList = jsonHelper.convertValueToList(orgResponse.entities, EntityTransferOrganizations.class);
                if(orgList.size() > 0) {

                    // fetch all address for default org
                    CommonV1ListRequest addressRequest = new CommonV1ListRequest();
                    List<Object> addressField = new ArrayList<>(List.of("OrgId"));
                    List<Object> addressCriteria = new ArrayList<>(List.of(addressField, "=", orgList.get(0).getId()));
                    addressRequest.setCriteriaRequests(addressCriteria);
                    V1DataResponse addressResponse = v1Service.addressList(addressRequest);
                    List<EntityTransferAddress> addressList = jsonHelper.convertValueToList(addressResponse.entities, EntityTransferAddress.class);

                    awbShipmentInfo.setIssuingAgentName(StringUtility.toUpperCase(orgList.get(0).getFullName()));
                    awbShipmentInfo.setIataCode(awbShipmentInfo.getIataCode() == null ? orgList.get(0).getAgentIATACode() : awbShipmentInfo.getIataCode());
                    awbShipmentInfo.setAgentCASSCode(awbShipmentInfo.getAgentCASSCode() == null ?
                            orgList.get(0).getAgentCASSCode() : awbShipmentInfo.getAgentCASSCode());

                    if(addressList == null || addressList.isEmpty()) {
                        awbShipmentInfo.setIssuingAgentAddress(StringUtility.toUpperCase(getFormattedAddress(orgList.get(0))));
                    } else {
                        EntityTransferAddress address = addressList.stream().filter(x -> Objects.equals(x.getAddressShortCode(), "Default")).findFirst().orElse(addressList.get(0));
                        if(address != null) {
                            var addressMap = jsonHelper.convertJsonToMap(jsonHelper.convertToJson(address));
                            awbShipmentInfo.setIssuingAgentAddress(AwbUtility.constructAddress(addressMap));
                        }
                    }
                    if(awbCargoInfo != null) {
                        String country = orgList.get(0) != null ?
                                orgList.get(0).getCountry() : null;
                        if (country != null)
                            awbCargoInfo.setCustomOriginCode(getCountryCode(country));
                        String city = orgList.get(0) != null ? stringValueOf(orgList.get(0).getCity()) : null;
                        if(StringUtility.isNotEmpty(city))
                            executedAt = setUnLocationDataWithDiarcties(city);
                        else
                            executedAt = null;
                    }
                }

            } catch (Exception e) {
                throw new RunnerException(String.format("Failed to fetch organization data for default org : %s", tenantModel.DefaultOrgId));
            }
        }
    }

    public String validateAwb(Awb awb) throws RunnerException {
        List<String> errors = new ArrayList<>();
        String awbType = awb.getAwbShipmentInfo().getEntityType();
        if(Constants.MAWB.equalsIgnoreCase(awbType)) {
            boolean allHawbsGenerated = true;
            var id = awb.getConsolidationId();
            if(id == null) {
                throw new RunnerException("ID can't be null, please provide a valid input !");
            }

            List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByConsolidationId(id);
            List<Long> shipmentIdList = consoleShipmentMappings.stream().map(ConsoleShipmentMapping::getShipmentId).toList();
            List<MawbHawbLink> mawbHawbLinks = mawbHawbLinkDao.findByMawbId(awb.getId());
            Set<Long> shipmentAwbIdSet = mawbHawbLinks.stream().map(MawbHawbLink::getHawbId).collect(Collectors.toSet());
            // Check whether HAWB is generated for all the linked shipments
            for(var shipmentId : shipmentIdList) {
                List<Awb> response = awbDao.findByShipmentIdList(Arrays.asList(shipmentId));
                if (Objects.isNull(response) || response.size() == 0) {
                    allHawbsGenerated = false;
                    break;
                }
                else if(!shipmentAwbIdSet.contains(response.get(0).getId())) {
                    errors.add("Additional Shipments have been attached, please reset data as required.");
                    break;
                }
            }
            if(Boolean.FALSE.equals(awb.getAirMessageResubmitted()) && !Objects.isNull(awb.getAirMessageStatus()))
                errors.add(AwbConstants.RESUBMIT_FWB_VALIDATION);

            if(!allHawbsGenerated)
                throw new RunnerException(AwbConstants.GENERATE_HAWB_BEFORE_MAWB_EXCEPTION);
        }
        else {
            // For HAWB/DMAWB
            if(Boolean.FALSE.equals(awb.getAirMessageResubmitted()) && !Objects.isNull(awb.getAirMessageStatus()))
                errors.add(Constants.DMAWB.equalsIgnoreCase(awbType) ? AwbConstants.RESUBMIT_FWB_VALIDATION : AwbConstants.RESUBMIT_FZB_VALIDATION);
        }

        return errors.size() > 0 ? errors.toString() : null;
    }

    private String fetchChargeCodes(String paymentTerms) {
        if(paymentTerms == null)
            return null;
        String chargeCode = null;
        if(paymentTerms.equals(Constants.PREPAID_DESC)){
            chargeCode = Constants.PREPAID_CODE;
        }else if(paymentTerms.equals(Constants.COLLECT_DESC)){
            chargeCode = Constants.COLLECT_CODE;
        }else if(paymentTerms.equals(Constants.COLLECT_PREPAID_DESC_CODE)){
            chargeCode = Constants.COLLECT_PREPAID_DESC_CODE;
        }else if(paymentTerms.equals(Constants.PREPAID_COLLECT_DESC_CODE)){
            chargeCode = Constants.PREPAID_COLLECT_DESC_CODE;
        }
        return chargeCode;
    }

    @Override
    public ResponseEntity<IRunnerResponse> validateIataAgent(Boolean fromShipment, Optional<Long> consolidationId) {
        TenantModel tenantModel = jsonHelper.convertValue(v1Service.retrieveTenant().getEntity(), TenantModel.class);
        IataAgentResponse res;
        if(tenantModel.IATAAgent && !Strings.isNullOrEmpty(tenantModel.AgentIATACode)
                && !Strings.isNullOrEmpty(tenantModel.AgentCASSCode) && !Strings.isNullOrEmpty(tenantModel.PIMAAddress)){
            if(Boolean.TRUE.equals(fromShipment)){
                res = IataAgentResponse.builder().iataAgent(true).message("FWB will be sent before printing, do you want to proceed?").build();
            } else {
                if (consolidationId.isPresent())
                    checkForHawbPrinted(consolidationId.get());
                res = IataAgentResponse.builder().iataAgent(true).message("FWB & FZB  will be sent before printing, do you want to proceed?").build();
            }
        } else if (tenantModel.IATAAgent && !Strings.isNullOrEmpty(tenantModel.AgentIATACode)
                && !Strings.isNullOrEmpty(tenantModel.AgentCASSCode) && Strings.isNullOrEmpty(tenantModel.PIMAAddress)) {
            res = IataAgentResponse.builder().iataAgent(false).message("PIMA address is not added in the branch settings, FWB and FZB will not be sent").build();
        } else {
            res = IataAgentResponse.builder().iataAgent(false).build();
        }

        return ResponseHelper.buildSuccessResponse(res);
    }

    @Override
    public ResponseEntity<IRunnerResponse> getFnmStatusMessage(Optional<Long> shipmentId, Optional<Long> consolidaitonId) {

        Awb masterAwb = null;
        String entityType = "";
        List<Awb> awbList = null;
        StringBuilder responseStatusMessage = new StringBuilder();
        Boolean fnMstatus = false;
        if(shipmentId.isPresent()) {
            awbList = awbDao.findByShipmentId(shipmentId.get());
        }
        else if(consolidaitonId.isPresent()) {
            awbList = awbDao.findByConsolidationId(consolidaitonId.get());
        }
        else {
            // Throw some error both can't be null
            // or send empty response object
            return ResponseHelper.buildSuccessResponse();
        }

        if(!Objects.isNull(awbList) && !awbList.isEmpty()) {
            masterAwb = awbList.get(0);
            entityType = masterAwb.getAwbShipmentInfo().getEntityType();
        }
        FnmStatusMessageResponse fnmStatusMessageResponse = new FnmStatusMessageResponse();

        // Fetch the logs for all Awb entities (master and its linked Hawb)
        // Update the response as per AirMessagingLogs
        switch(entityType) {
            case Constants.HAWB -> fnmAcknowledgementHawb(masterAwb, fnmStatusMessageResponse);
            case Constants.DMAWB -> fnmAcknowledgementMawb(masterAwb, fnmStatusMessageResponse);
            case Constants.MAWB -> fnmAcknowledgementMawb(masterAwb, fnmStatusMessageResponse);
        }

        return ResponseHelper.buildSuccessResponse(fnmStatusMessageResponse);
    }

    private void fnmAcknowledgementHawb(Awb awb, FnmStatusMessageResponse fnmStatusMessageResponse) {
        Boolean fnmStatus = null;
        StringBuilder responseStatusMessage = new StringBuilder();

        var statusLog = airMessagingLogsService.getRecentLogForEntityGuid(awb.getGuid());
        if (statusLog == null)
            return;
        if(Objects.equals(statusLog.getStatus(), AirMessagingStatus.FAILED.name())) {
            fnmStatus = false;
            responseStatusMessage.append(String.format(AirMessagingLogsConstants.SHIPMENT_FNM_FAILURE_ERROR, statusLog.getErrorMessage()));
        }
        if(Objects.equals(statusLog.getStatus(), AirMessagingStatus.SUCCESS.name())) {
            fnmStatus = true;
            responseStatusMessage.append("FZB submission is accepted");
        }

        fnmStatusMessageResponse.setFnmStatus(fnmStatus);
        fnmStatusMessageResponse.setResponse(responseStatusMessage.toString());
    }
    private void fnmAcknowledgementMawb(Awb mawb, FnmStatusMessageResponse fnmStatusMessageResponse) {
        var mawbStatusLog = airMessagingLogsService.getRecentLogForEntityGuid(mawb.getGuid());
        String awbType = mawb.getAwbShipmentInfo().getEntityType();
        Boolean fnmStatus = null;
        StringBuilder responseStatusMessage = new StringBuilder();

        if (mawbStatusLog == null)
            return;

        // Stores shipment ID of failed Hawbs
        List<Long> failedShipmentHawbs = new ArrayList<>();
        List<Awb> linkedHawb = getLinkedAwbFromMawb(mawb.getId());
        int successHawbCount = 0;

        for(var hawb : linkedHawb) {
            var statusLog = airMessagingLogsService.getRecentLogForEntityGuid(hawb.getGuid());

            if(statusLog == null) continue;

            if(Objects.equals(statusLog.getStatus(), AirMessagingStatus.FAILED.name())) {
                failedShipmentHawbs.add(hawb.getShipmentId());
            }
            if(Objects.equals(statusLog.getStatus(), AirMessagingStatus.SUCCESS.name())) {
                successHawbCount += 1;
            }
        }


        boolean failedMawb = Objects.equals(mawbStatusLog.getStatus(), AirMessagingStatus.FAILED.name());
        boolean failedHawb = !failedShipmentHawbs.isEmpty();

        if(Objects.equals(mawbStatusLog.getStatus(), AirMessagingStatus.SUCCESS.name())) {
            fnmStatus = true;
            if(awbType.equals(Constants.DMAWB))
                responseStatusMessage.append("FZB submission is accepted");
            else if(successHawbCount == linkedHawb.size())
                responseStatusMessage.append("FWB and FZB submissions are accepted");
        }

        // Cases to generate the error Log
        if(!failedMawb && !failedHawb) {
        }
        if(failedHawb) {
            fnmStatus = false;
            // if failedShipmentHawb size > 0 fetch ShipmentID for those shipments
            var shipmentDetailsPage = shipmentDao.findShipmentsByIds(failedShipmentHawbs.stream().collect(Collectors.toSet()));
            String shipmentNumbersString = "";
            if(!shipmentDetailsPage.isEmpty()) {
                List<String> shipmentNumbers = shipmentDetailsPage.stream().map(ShipmentDetails::getShipmentId).toList();
                shipmentNumbersString = String.join(" ", shipmentNumbers);
            }

            if(failedMawb) {
                responseStatusMessage.append(String.format(
                        AirMessagingLogsConstants.CONSOLIDATION_FNM_MAWB_FAILURE_HAWB_FAILURE_ERROR, shipmentNumbersString)
                );
            }
            else {
                responseStatusMessage.append(String.format(
                        AirMessagingLogsConstants.CONSOLIDATION_FNM_MAWB_SUCCESS_HAWB_FAILURE_ERROR, shipmentNumbersString)
                );
            }
        }
        // !failedHawb && failedMawb
        else if(!failedHawb && failedMawb){
            fnmStatus = false;
            responseStatusMessage.append(String.format(
                    AirMessagingLogsConstants.CONSOLIDATION_FNM_MAWB_FAILURE_HAWB_SUCCESS_ERROR, mawbStatusLog.getErrorMessage())
            );
        }

        fnmStatusMessageResponse.setFnmStatus(fnmStatus);
        fnmStatusMessageResponse.setResponse(responseStatusMessage.toString());
    }

    @Override
    public ResponseEntity<IRunnerResponse> getFetchIataRates(CommonRequestModel commonRequestModel) throws RunnerException {
        IataFetchRateRequest iataFetchRateRequest = (IataFetchRateRequest) commonRequestModel.getData();
        List<String> emptyFieldsError = new ArrayList<>();
        if(iataFetchRateRequest.getFlightCarrier() == null || iataFetchRateRequest.getFlightCarrier().isEmpty()){
            emptyFieldsError.add("Fligt Carrier");
        }
        if(iataFetchRateRequest.getChargeableWeight() == null){
            emptyFieldsError.add("Chargeable Weight");
        }
        if(iataFetchRateRequest.getOriginPort() == null || iataFetchRateRequest.getOriginPort().isEmpty()){
            emptyFieldsError.add("Origin Airport");
        }
        if(iataFetchRateRequest.getDestinationPort() == null || iataFetchRateRequest.getDestinationPort().isEmpty()){
            emptyFieldsError.add("Destination Airport");
        }
        if(iataFetchRateRequest.getCurrency() == null || iataFetchRateRequest.getCurrency().isEmpty()){
            emptyFieldsError.add("Currency(in Cargo Information)");
        }
        if(!emptyFieldsError.isEmpty()){
            String errorString = String.join(", ", emptyFieldsError);
            throw new ValidationException("Please add " + errorString + " and retry");
        }
        List<String> unlocoRequests = new ArrayList<>(List.of(iataFetchRateRequest.getOriginPort(), iataFetchRateRequest.getDestinationPort()));
        List<String> carrierRequests = new ArrayList<>(List.of(iataFetchRateRequest.getFlightCarrier()));

        Map<String, UnlocationsResponse> unlocationsMap = masterDataUtils.getLocationData(new HashSet<>(unlocoRequests));
        Map<String, EntityTransferCarrier> carriersMap = masterDataUtils.fetchInBulkCarriers(carrierRequests);
        String origin = unlocationsMap.containsKey(iataFetchRateRequest.getOriginPort())? unlocationsMap.get(iataFetchRateRequest.getOriginPort()).getIataCode() : null;
        String destination = unlocationsMap.containsKey(iataFetchRateRequest.getDestinationPort())? unlocationsMap.get(iataFetchRateRequest.getDestinationPort()).getIataCode(): null;
        String carrier = carriersMap.containsKey(iataFetchRateRequest.getFlightCarrier())? carriersMap.get(iataFetchRateRequest.getFlightCarrier()).IATACode : null;
        List<String> emptyIataError = new ArrayList<>();
        if (origin == null || origin.isEmpty()) {
            emptyIataError.add("Origin Airport");
        }
        if (destination == null || destination.isEmpty()){
            emptyIataError.add("Destination Airport");
        }
        if (carrier == null || carrier.isEmpty()) {
            emptyIataError.add("Fligt Carrier");
        }
        if(!emptyIataError.isEmpty()) {
            throw new ValidationException("Please add IataCode in the follwing fields " + String.join(", ", emptyIataError) + " and retry");
        }

        TactBridgePayload tactBridgePayload = TactBridgePayload.builder()
                .origin(origin)
                .destination(destination)
                .carrier(carrier)
                .rateSource("GS")
                .rateType("CAR")
                .currency(iataFetchRateRequest.getCurrency())
                .build();
        log.info("TactPayload : "+ jsonHelper.convertToJson(tactBridgePayload));
        BridgeServiceResponse bridgeServiceResponse = (BridgeServiceResponse) bridgeServiceAdapter.requestTactResponse(CommonRequestModel.buildRequest(tactBridgePayload));
        if(bridgeServiceResponse.getExtraResponseParams().containsKey(AwbConstants.SERVICE_HTTP_STATUS_CODE) && !Objects.equals(bridgeServiceResponse.getExtraResponseParams().get(AwbConstants.SERVICE_HTTP_STATUS_CODE).toString(), "200") &&
                    !Objects.equals(bridgeServiceResponse.getExtraResponseParams().get(AwbConstants.SERVICE_HTTP_STATUS_CODE).toString(), "400")){
            log.error("Getting error from Iata while fetching rates due to: " + jsonHelper.convertToJson(bridgeServiceResponse));
            throw new RunnerException("Getting error from Iata while fetching rates");
        }

        IataTactRatesApiResponse iataTactRatesApiResponse = jsonHelper.convertValue(bridgeServiceResponse.getPayload(), IataTactRatesApiResponse.class);
        if(iataTactRatesApiResponse != null && iataTactRatesApiResponse.getResponseType() != null && Objects.equals(iataTactRatesApiResponse.getResponseType(), "validation-failed")
                && iataTactRatesApiResponse.getErrors() != null && !iataTactRatesApiResponse.getErrors().isEmpty()){
            List<String> errors = iataTactRatesApiResponse.getErrors().stream().map(IataTactRatesApiResponse.Errors::getMessage).toList();
            throw new ValidationException(String.join("\r\n", errors));
        }

        if(iataTactRatesApiResponse != null && iataTactRatesApiResponse.getRates() != null && !iataTactRatesApiResponse.getRates().isEmpty()){
            IataTactRatesApiResponse.StandardCharge standardCharge = iataTactRatesApiResponse.getRates().get(0).getStandardCharge();
            if(standardCharge.getWeightBreak() != null && !standardCharge.getWeightBreak().isEmpty()){
                Map<BigDecimal, BigDecimal> weightBreakMap = standardCharge.getWeightBreak().stream().collect(Collectors.toMap(IataTactRatesApiResponse.WeightBreak::getWeightMeasure, IataTactRatesApiResponse.WeightBreak::getCharge));
                List<BigDecimal> weightList = standardCharge.getWeightBreak().stream().map(IataTactRatesApiResponse.WeightBreak::getWeightMeasure).sorted().toList();
                BigDecimal minimumCharge = standardCharge.getMinimumCharge();
                BigDecimal normalCharge = standardCharge.getNormalCharge();
                BigDecimal minWeightValue = weightList.get(0);
                BigDecimal chargeableWeight = iataFetchRateRequest.getChargeableWeight();
                if(chargeableWeight.compareTo(minWeightValue) < 0) {
                    BigDecimal rate = chargeableWeight.multiply(normalCharge);
                    if(rate.compareTo(minimumCharge) < 0){
                        IataFetchRateResponse iataFetchRateResponse = IataFetchRateResponse.builder()
                                .rateClass(RateClass.M.getId())
                                .rateCharge(minimumCharge)
                                .build();
                        return ResponseHelper.buildSuccessResponse(iataFetchRateResponse);
                    }
                    IataFetchRateResponse iataFetchRateResponse = IataFetchRateResponse.builder()
                            .rateClass(RateClass.N.getId())
                            .rateCharge(normalCharge)
                            .build();
                    return ResponseHelper.buildSuccessResponse(iataFetchRateResponse);
                }
                int size = weightList.size();
                for(int i = 0; i < size; i++) {
                    if(i == (size-1) && chargeableWeight.compareTo(weightList.get(i)) >= 0){
                        IataFetchRateResponse iataFetchRateResponse = IataFetchRateResponse.builder()
                                .rateClass(RateClass.Q.getId())
                                .rateCharge(weightBreakMap.get(weightList.get(i)))
                                .build();
                        return ResponseHelper.buildSuccessResponse(iataFetchRateResponse);
                    }
                    if(chargeableWeight.compareTo(weightList.get(i)) >= 0 &&
                            chargeableWeight.compareTo(weightList.get(i + 1)) < 0){
                        IataFetchRateResponse iataFetchRateResponse = IataFetchRateResponse.builder()
                                .rateClass(RateClass.Q.getId())
                                .rateCharge(weightBreakMap.get(weightList.get(i)))
                                .build();
                        return ResponseHelper.buildSuccessResponse(iataFetchRateResponse);
                    }
                }
            }
        }

        IataFetchRateResponse iataFetchRateResponse = IataFetchRateResponse.builder().error("IATA did not return any value - please add the rate/ rate class manually").build();
        return ResponseHelper.buildSuccessResponse(iataFetchRateResponse);
    }

    private void checkForHawbPrinted(Long consolidationId) {
        List<ConsoleShipmentMapping> consoleShipmentMappingList = consoleShipmentMappingDao.findByConsolidationId(consolidationId);
        List<String> errorShipments = new ArrayList<>();
        if (!consoleShipmentMappingList.isEmpty()) {
            var shipmentsList = shipmentDao.getShipmentNumberFromId(consoleShipmentMappingList.stream().map(ConsoleShipmentMapping::getShipmentId).toList());
            var shipmentIdToNumberMap = shipmentsList.stream().collect(Collectors.toMap(ShipmentDetails::getId, ShipmentDetails::getShipmentId));
            var awbList = awbDao.findByShipmentIdsByQuery(shipmentsList.stream().map(ShipmentDetails::getId).toList());
            for (var awb : awbList) {
                if (!Objects.equals(awb.getPrintType(), PrintType.ORIGINAL_PRINTED))
                    errorShipments.add(shipmentIdToNumberMap.get(awb.getShipmentId()));
                shipmentIdToNumberMap.remove(awb.getShipmentId());
            }
            for (var entry : shipmentIdToNumberMap.entrySet())
                errorShipments.add(entry.getValue());

            if (!errorShipments.isEmpty())
                throw new ValidationException(String.format(ErrorConstants.HAWB_NOT_GENERATED_ERROR, String.join(", ", errorShipments)));
        }
    }
}