package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.adapters.impl.BridgeServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerPartialListResponse;
import com.dpw.runner.shipment.services.config.SyncConfig;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.PackSummaryResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ShipmentMeasurementDetailsDto;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.request.awb.*;
import com.dpw.runner.shipment.services.dto.request.bridgeService.TactBridgePayload;
import com.dpw.runner.shipment.services.dto.request.reportService.CompanyDto;
import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.dto.response.bridgeService.BridgeServiceResponse;
import com.dpw.runner.shipment.services.dto.v1.request.V1RetrieveRequest;
import com.dpw.runner.shipment.services.dto.v1.response.AddressDataV1;
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
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.retry.support.RetryTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import javax.servlet.http.HttpServletRequest;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.BiConsumer;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.COUNTRY;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.MAWB;
import static com.dpw.runner.shipment.services.commons.constants.AwbConstants.*;
import static com.dpw.runner.shipment.services.commons.constants.Constants.VOLUME_UNIT_M3;
import static com.dpw.runner.shipment.services.commons.constants.Constants.WEIGHT_UNIT_KG;
import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.isStringNullOrEmpty;

@SuppressWarnings("ALL")
@Service
@Slf4j
public class AwbService implements IAwbService {

    private static final String RA_KC_VALIDATION_MESSAGE = "You cannot generate the AWB without adding the screening/ Security status for RA KC %s";
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
    RestTemplate restTemplate;
    @Autowired
    IAwbSync awbSync;
    @Autowired
    IShipmentService shipmentService;
    @Autowired
    IShipmentSync shipmentSync;
    @Autowired
    ExecutorService executorService;
    @Autowired
    private IConsolidationService consolidationService;
    @Autowired
    private PackingService packingService;
    @Autowired
    private IAwbDao awbDao;
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
    private IShipmentSettingsDao shipmentSettingsDao;
    @Autowired
    private UnitConversionUtility unitConversionUtility;
    @Autowired
    private SyncConfig syncConfig;
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
    private PartialFetchUtils partialFetchUtils;
    @Autowired
    private CommonUtils commonUtils;
    @Autowired
    private V1ServiceUtil v1ServiceUtil;
    @Autowired
    private AwbService self;
    private List<String> attachedShipmentDescriptions = new ArrayList<>();
    private BigDecimal totalVolumetricWeightOfAwbPacks = new BigDecimal(0);

    private String iataCode;
    private String executedAt;

    public static String convertIpFormat(String ip) {
        if (ip == null || ip.isEmpty()) {
            return "";
        }
        // Replace "::" with a single "-"
        String formattedIp = ip.replace("::", "-");
        // Replace remaining "." and ":" with "-"
        return formattedIp.replaceAll("[.:]", "-");
    }

    private static BigDecimal roundOffAirShipment(BigDecimal charge) {
        BigDecimal roundedCharge = CommonUtils.roundBigDecimal(charge, 0, RoundingMode.FLOOR);
        if (charge.subtract(new BigDecimal("0.50")).compareTo(roundedCharge) <= 0
                && charge.compareTo(roundedCharge) != 0) {
            charge = roundedCharge.add(new BigDecimal("0.50"));
        } else {
            charge = CommonUtils.roundBigDecimal(charge, 0, RoundingMode.CEILING);
        }
        return charge;
    }

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
            syncAwb(awb, SaveStatus.CREATE);

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
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

    private void syncAwb(Awb awb, SaveStatus saveStatus) {
        try {
            awbSync.sync(awb, saveStatus);
        } catch (Exception e) {
            log.error(SyncingConstants.ERROR_PERFORMING_AWB_SYNC, e);
        }
    }

    public ResponseEntity<IRunnerResponse> updateAwb(CommonRequestModel commonRequestModel) {
        String responseMsg;
        AwbRequest request = (AwbRequest) commonRequestModel.getData();

        Awb awb = convertRequestToEntity(request);
        awb.setAwbNumber(awb.getAwbShipmentInfo().getAwbNumber());
        commonUtils.checkForMandatoryHsCodeForUAE(awb);

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
            if (awb.getAwbShipmentInfo().getEntityType().equals(Constants.MAWB)) {
                var optionalConsole = consolidationDetailsDao.findById(awb.getConsolidationId());
                if (optionalConsole.isPresent() && Boolean.TRUE.equals(optionalConsole.get().getInterBranchConsole()))
                    commonUtils.setInterBranchContextForHub();
                List<AwbPackingInfo> awbPackingInfoList = awb.getAwbPackingInfo();
                awb.setAwbPackingInfo(null);
                updateAwbPacking(awb.getId(), awbPackingInfoList);
            } else {
                setAwbNumberFromPackingInfo(awb);
                awbDao.updateSciFieldFromHawb(awb, oldEntity.get(), false, id);
            }
            setOciInfoInAwb(awb);
            awb = awbDao.save(awb);

            syncAwb(awb, SaveStatus.UPDATE);

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
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

    private void setOciInfoInAwb(Awb awb) {
        if (awb.getOciInfo() != null) {
            if (awb.getOciInfo().getOtherIdentityInfo() != null) {
                if (awb.getOciInfo().getOtherIdentityInfo().getIrIpAddress() != null &&  awb.getOciInfo().getOtherIdentityInfo().getIrIpAddress().isEmpty()) {
                    awb.getOciInfo().getOtherIdentityInfo().setIrIpAddress(convertIpFormat(getClientIp()));
                }
                if (awb.getOciInfo().getOtherIdentityInfo().getIaIpAddress() != null &&  awb.getOciInfo().getOtherIdentityInfo().getIaIpAddress().isEmpty()) {
                    awb.getOciInfo().getOtherIdentityInfo().setIaIpAddress(convertIpFormat(getClientIp()));
                }
            } else {
                OtherIdentityInfo otherIdentityInfo = new OtherIdentityInfo();
                otherIdentityInfo.setIrIpAddress(convertIpFormat(getClientIp()));
                awb.getOciInfo().setOtherIdentityInfo(otherIdentityInfo);
            }
        }
    }

    private String getClientIp() {
        ServletRequestAttributes attrs = (ServletRequestAttributes) RequestContextHolder.getRequestAttributes();
        if (attrs != null) {
            HttpServletRequest request = attrs.getRequest();
            String ip = request.getHeader("X-Forwarded-For"); // Handle proxies
            if (ip == null || ip.isEmpty() || "unknown".equalsIgnoreCase(ip)) {
                ip = request.getRemoteAddr(); // Get direct IP
            }
            return ip;
        }
        return "UNKNOWN";
    }

    private void setAwbNumberFromPackingInfo(Awb awb) {
        if (awb.getAwbPackingInfo() != null && !awb.getAwbPackingInfo().isEmpty()) {
            for (var i : awb.getAwbPackingInfo()) {
                i.setAwbNumber(awb.getAwbNumber());
            }
        }
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

        if (!errors.isEmpty())
            throw new ValidationException(String.join(",", errors));
    }

    private void updateAwbPacking(long mawbId, List<AwbPackingInfo> awbPackingInfoList) throws RunnerException {
        if (awbPackingInfoList == null) {
            return;
        }

        Map<String, List<AwbPackingInfo>> dataMap = new HashMap<>();

        for (AwbPackingInfo awbPackingInfo : awbPackingInfoList) {
            dataMap.putIfAbsent(awbPackingInfo.getAwbNumber(), new ArrayList<>());
            dataMap.get(awbPackingInfo.getAwbNumber()).add(awbPackingInfo);
        }

        List<Awb> awbList = getLinkedAwbFromMawb(mawbId);
        if (awbList != null) {
            for (Awb awb : awbList) {
                List<AwbPackingInfo> hawbPacks = dataMap.get(awb.getAwbNumber());
                awb.setAwbPackingInfo(hawbPacks);
                calculateGoodsDescription(awb.getAwbGoodsDescriptionInfo().get(0), hawbPacks, commonUtils.getShipmentSettingFromContext(), new HashMap<>(), true);
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

            List<String> includeColumns = request.getIncludeColumns();

            List<Awb> awbList = awbPage.getContent();

            if (awbList != null) {
                ShipmentSettingsDetails tenantSettings = commonUtils.getShipmentSettingFromContext();
                for (Awb awb : awbList) {
                    if (awb.getAwbShipmentInfo().getEntityType().equals(Constants.MAWB)) {
                        processMawbRequestFromGenerateAwbButton(awb, request, tenantSettings);

                    } else {
                        processAwbRequestFromGenerateAwbButton(awb, request, tenantSettings);
                    }
                }
            }

            if (includeColumns == null || includeColumns.isEmpty()) {
                return ResponseHelper.buildListSuccessResponse(
                        convertEntityListToDtoList(awbList),
                        awbPage.getTotalPages(),
                        awbPage.getTotalElements());
            } else {

                List<IRunnerResponse> filteredList = getFilteredList(awbList, request);
                return ResponseHelper.buildListSuccessResponse(
                        filteredList,
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

    private List<IRunnerResponse> getFilteredList(List<Awb> awbList, FetchAwbListRequest request) {
        List<IRunnerResponse> filteredList = new ArrayList<>();
        for (var curr : convertEntityListToDtoList(awbList)) {
            RunnerPartialListResponse res = new RunnerPartialListResponse();
            res.setData(partialFetchUtils.fetchPartialListData(curr, request.getIncludeColumns()));
            filteredList.add(res);
        }
        return filteredList;
    }

    private void processAwbRequestFromGenerateAwbButton(Awb awb, FetchAwbListRequest request, ShipmentSettingsDetails tenantSettings) throws RunnerException {
        if (request.getFromGenerateAwbButton() != null && request.getFromGenerateAwbButton()) {
            ShipmentDetails shipmentDetails = shipmentDao.findById(awb.getShipmentId()).orElse(null);
            V1TenantSettingsResponse tenantSettingsResponse = commonUtils.getCurrentTenantSettings();
            shipmentService.validateRaKcDetails(shipmentDetails, tenantSettingsResponse);
        }
        if (request.getFromGenerateAwbButton() != null && request.getFromGenerateAwbButton()
                && tenantSettings != null && ((tenantSettings.getRestrictAWBEdit() != null
                && tenantSettings.getRestrictAWBEdit()) || (tenantSettings.getAutoUpdateShipmentAWB() != null && tenantSettings.getAutoUpdateShipmentAWB()))) {
            try {
                CreateAwbRequest req = CreateAwbRequest.builder().ShipmentId(awb.getShipmentId()).AwbType(awb.getAwbShipmentInfo().getEntityType()).build();
                self.partialAutoUpdateAwb(CommonRequestModel.buildRequest(req));
            } catch (Exception ex) {
                log.error("Error while updating Hawb due to: " + ex.getMessage());
            }
        }
    }

    private void processMawbRequestFromGenerateAwbButton(Awb awb, FetchAwbListRequest request, ShipmentSettingsDetails tenantSettings) throws RunnerException {
        if (request.getFromGenerateAwbButton() != null && request.getFromGenerateAwbButton()) {
            ConsolidationDetails consolidationDetails = consolidationDetailsDao.findById(awb.getConsolidationId()).orElse(null);
            V1TenantSettingsResponse tenantSettingsResponse = commonUtils.getCurrentTenantSettings();
            consolidationService.validateRaKcForConsol(consolidationDetails, tenantSettingsResponse);
        }
        if (request.getFromGenerateAwbButton() != null && request.getFromGenerateAwbButton()
                && tenantSettings != null && ((tenantSettings.getRestrictAWBEdit() != null
                && tenantSettings.getRestrictAWBEdit()) || (tenantSettings.getAutoUpdateShipmentAWB() != null && tenantSettings.getAutoUpdateShipmentAWB()))) {
            try {
                CreateAwbRequest req = CreateAwbRequest.builder().ConsolidationId(awb.getConsolidationId()).AwbType(awb.getAwbShipmentInfo().getEntityType()).build();
                self.partialAutoUpdateMawb(CommonRequestModel.buildRequest(req));
            } catch (Exception ex) {
                log.error("Error while updating Mawb due to: " + ex.getMessage());
            }
        }

        getMawnLinkPacks(awb, false, null);
    }

    //Calculate mawb packs
    @Override
    public Awb getMawnLinkPacks(Awb awb, boolean syncGoodsDescription, List<Awb> linkedHawb) {
        try {
            ShipmentSettingsDetails tenantSettings = commonUtils.getShipmentSettingFromContext();
            if(linkedHawb == null)
                linkedHawb = getLinkedAwbFromMawb(awb.getId());
            List<AwbPackingInfo> linkedPacks = new ArrayList<>();
            for (var hawb : linkedHawb) {
                if (hawb.getAwbPackingInfo() != null) {
                    hawb.setTenantId(hawb.getTenantId());
                    for (AwbPackingInfo pack : hawb.getAwbPackingInfo()) {
                        pack.setTenantId(hawb.getTenantId());
                        linkedPacks.add(pack);
                    }
                }
            }
            awb.setAwbPackingInfo(linkedPacks);
            if (syncGoodsDescription && awb.getAwbGoodsDescriptionInfo() != null && !awb.getAwbGoodsDescriptionInfo().isEmpty()) {
                calculateGoodsDescription(awb.getAwbGoodsDescriptionInfo().get(0), linkedPacks, tenantSettings, new HashMap<>(), !linkedPacks.isEmpty());
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
            if (awb.get().getAwbShipmentInfo().getEntityType().equals(Constants.MAWB)) {
                ShipmentSettingsDetails tenantSettings = getTenantSettings();
                List<AwbPackingInfo> linkedPacks = getLinkedHawbPackingInfos(awb.get());
                if (awb.get().getAwbGoodsDescriptionInfo() != null && !awb.get().getAwbGoodsDescriptionInfo().isEmpty()) {
                    calculateGoodsDescription(awb.get().getAwbGoodsDescriptionInfo().get(0), linkedPacks, tenantSettings, new HashMap<>(), true);
                }
            }
            AwbResponse response = convertEntityToDto(awb.get());

            if (request.getIncludeColumns() == null || request.getIncludeColumns().isEmpty())
                return ResponseHelper.buildSuccessResponse(response);
            else
                return ResponseHelper.buildSuccessResponse(partialFetchUtils.fetchPartialListData(response, request.getIncludeColumns()));

        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private ShipmentSettingsDetails getTenantSettings() {
        List<ShipmentSettingsDetails> tenantSettingsList = shipmentSettingsDao.getSettingsByTenantIds(Arrays.asList(UserContext.getUser().TenantId));
        ShipmentSettingsDetails tenantSettings = null;
        if (tenantSettingsList != null && !tenantSettingsList.isEmpty()) {
            tenantSettings = tenantSettingsList.get(0);
        }
        return tenantSettings;
    }

    private List<AwbPackingInfo> getLinkedHawbPackingInfos(Awb awb) {
        List<Awb> linkedHawb = getLinkedAwbFromMawb(awb.getId());
        List<AwbPackingInfo> linkedPacks = new ArrayList<>();
        for (var hawb : linkedHawb) {
            if (hawb.getAwbPackingInfo() != null) {
                linkedPacks.addAll(hawb.getAwbPackingInfo());
            }
        }
        awb.setAwbPackingInfo(linkedPacks);
        return linkedPacks;
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
            List<AwbPackingInfo> mawbPackingInfo = new ArrayList<>();
            List<Long> shipmentDetailsIdList = consolidationDetails.getShipmentsList()
                    .stream()
                    .map(ShipmentDetails::getId)
                    .toList();
            List<Awb> awbList = awbDao.findByShipmentIdList(shipmentDetailsIdList);
            Map<Long, Awb> shipmentIdToAwbMap = awbList.stream()
                    .collect(Collectors.toMap(Awb::getShipmentId, awb1 -> awb1));
            if (awbList.size() != shipmentDetailsIdList.size()) {
                throw new ValidationException(AwbConstants.GENERATE_HAWB_BEFORE_MAWB_EXCEPTION);
            }
            for (Long shipmentId : shipmentDetailsIdList) {
                Awb linkAwb = shipmentIdToAwbMap.get(shipmentId);
                if (linkAwb.getAwbPackingInfo() != null) {
                    mawbPackingInfo.addAll(linkAwb.getAwbPackingInfo());
                }
            }

            // save awb details
            awb = generateMawb(request, consolidationDetails, mawbPackingInfo);
            if (awbList != null && !awbList.isEmpty()) {
                updateSciFieldFromMawb(awb, awbList);
                getMawnLinkPacks(awb, true, awbList);
            }
            awb = awbDao.save(awb);
            syncAwb(awb, SaveStatus.CREATE);

            // map mawb and hawb affter suuccessful save
            linkHawbMawb(awb, awbList, consolidationDetails.getInterBranchConsole());

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
            updateGoodsAndPacks(request);
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
        for (var i : linkedHawb) {
            allHawbPacks.addAll(i.getAwbPackingInfo());
        }

        // Get tenant settings
        ShipmentSettingsDetails tenantSettings = commonUtils.getShipmentSettingFromContext();

        if (allHawbPacks.isEmpty() && !Boolean.TRUE.equals(tenantSettings.getConsolidationLite())) {
            updateGoodsDescForMawb(mawb);
        } else if (!allHawbPacks.isEmpty()) {
            calculateAndUpdateGoodsPacksMawb(allHawbPacks, mawb, tenantSettings);
        }
    }

    private void updateGoodsDescForMawb(Awb mawb) throws RunnerException {
        AwbGoodsDescriptionInfo awbGoodsDescriptionInfo = null;
        if (mawb.getAwbGoodsDescriptionInfo() != null && !mawb.getAwbGoodsDescriptionInfo().isEmpty()) {
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
        if (mawb.getAwbGoodsDescriptionInfo() != null && !mawb.getAwbGoodsDescriptionInfo().isEmpty()) {
            // V1 always fetches via FirstOrDefault method post repo list call
            mawbGoodsDescriptionInfo = mawb.getAwbGoodsDescriptionInfo().get(0);
            Map<String, List<AwbPackingInfo>> hawbPacksMap = new HashMap<>(); // map to store awbNumber -> packsList

            var pair = calculateGoodsDescription(mawbGoodsDescriptionInfo, allHawbPacks, tenantSettings, hawbPacksMap, true);

            // Can there be a scenario of multiple Goods information ?
            mawb.setAwbGoodsDescriptionInfo(List.of(pair.getRight()));
            saveHawbPacks(hawbPacksMap);
            awbDao.save(mawb);
        }
    }

    private Pair<BigDecimal, AwbGoodsDescriptionInfo> calculateGoodsDescription(AwbGoodsDescriptionInfo mawbGoodsDescriptionInfo, List<AwbPackingInfo> allHawbPacks, ShipmentSettingsDetails tenantSettings, Map<String, List<AwbPackingInfo>> hawbPacksMap, boolean isPackUpdate) throws RunnerException {
        Integer noOfPacks = 0;
        BigDecimal totalGrossVolumeOfMawbGood = BigDecimal.ZERO;
        BigDecimal totalGrossWeightOfMawbGood = BigDecimal.ZERO;
        BigDecimal chargeableWeightOfMawbGood = BigDecimal.ZERO;
        BigDecimal totalAmountOfMawbGood;
        String grossWeightUnit = WEIGHT_UNIT_KG;

        BigDecimal totalVolumetricWeight = BigDecimal.ZERO;

        if (allHawbPacks != null && !allHawbPacks.isEmpty()) {
            for (var i : allHawbPacks) {
                noOfPacks += Integer.parseInt(Objects.isNull(i.getPacks()) ? "0" : i.getPacks());
                try {
                    // Populate volume related fields
                    totalGrossVolumeOfMawbGood = getTotalGrossVolumeOfMawbGood(tenantSettings, i, totalGrossVolumeOfMawbGood);
                    // Populate weight related fields
                    totalGrossWeightOfMawbGood = getTotalGrossWeightOfMawbGood(tenantSettings, i, totalGrossWeightOfMawbGood);

                    processHawbPacksMap(hawbPacksMap, i);

                } catch (Exception e) {
                    log.error(e.getMessage());
                    throw new RunnerException(e.getMessage());
                }
            }
            if (tenantSettings != null && VOLUME_UNIT_M3.equalsIgnoreCase(tenantSettings.getVolumeChargeableUnit()) && WEIGHT_UNIT_KG.equalsIgnoreCase(tenantSettings.getWeightChargeableUnit())) {
                grossWeightUnit = WEIGHT_UNIT_KG;
                chargeableWeightOfMawbGood = totalGrossWeightOfMawbGood;
                BigDecimal volumetricWeightOfMawbGood = totalGrossVolumeOfMawbGood.multiply(BigDecimal.valueOf(Constants.AIR_FACTOR_FOR_VOL_WT));
                chargeableWeightOfMawbGood = chargeableWeightOfMawbGood.max(volumetricWeightOfMawbGood);
                totalVolumetricWeight = volumetricWeightOfMawbGood;
            }
        }

        if (isPackUpdate) {
            mawbGoodsDescriptionInfo.setNtrQtyGoods(mawbGoodsDescriptionInfo.getNtrQtyGoods());
            mawbGoodsDescriptionInfo.setGrossVolume(totalGrossVolumeOfMawbGood.setScale(3, RoundingMode.HALF_UP));
            mawbGoodsDescriptionInfo.setGrossVolumeUnit(VOLUME_UNIT_M3);
            mawbGoodsDescriptionInfo.setGrossWt(totalGrossWeightOfMawbGood);
            mawbGoodsDescriptionInfo.setGrossWtUnit(grossWeightUnit);
            mawbGoodsDescriptionInfo.setPiecesNo(noOfPacks);
            mawbGoodsDescriptionInfo.setChargeableWt(roundOffAirShipment(chargeableWeightOfMawbGood));
        }

        updateMawbTotalAmount(mawbGoodsDescriptionInfo);
        totalAmountOfMawbGood = mawbGoodsDescriptionInfo.getTotalAmount();
        mawbGoodsDescriptionInfo.setTotalAmount(totalAmountOfMawbGood);
        return Pair.of(totalVolumetricWeight, mawbGoodsDescriptionInfo);
    }

    private void updateMawbTotalAmount(AwbGoodsDescriptionInfo mawbGoodsDescriptionInfo) {
        if (mawbGoodsDescriptionInfo.getRateCharge() != null && mawbGoodsDescriptionInfo.getRateClass() != null) {
            if (mawbGoodsDescriptionInfo.getRateClass() == 1)
                mawbGoodsDescriptionInfo.setTotalAmount(mawbGoodsDescriptionInfo.getRateCharge());
            else
                mawbGoodsDescriptionInfo.setTotalAmount(mawbGoodsDescriptionInfo.getChargeableWt() != null ? mawbGoodsDescriptionInfo.getRateCharge().multiply(mawbGoodsDescriptionInfo.getChargeableWt()) : BigDecimal.ZERO);
        }
    }

    private void processHawbPacksMap(Map<String, List<AwbPackingInfo>> hawbPacksMap, AwbPackingInfo i) {
        if (hawbPacksMap.get(i.getAwbNumber()) == null) {
            hawbPacksMap.put(i.getAwbNumber(), new ArrayList<>());
        } else {
            List<AwbPackingInfo> existingPacks = hawbPacksMap.get(i.getAwbNumber());
            existingPacks.add(i);
            hawbPacksMap.put(i.getAwbNumber(), existingPacks);
        }
    }

    @SuppressWarnings("java:S2209")
    private BigDecimal getTotalGrossWeightOfMawbGood(ShipmentSettingsDetails tenantSettings, AwbPackingInfo i, BigDecimal totalGrossWeightOfMawbGood) throws RunnerException {
        if (i.getWeight() != null) {
            if (i.getWeightUnit() == null || i.getWeightUnit().isEmpty())
                totalGrossWeightOfMawbGood = totalGrossWeightOfMawbGood.add(convertToBigDecimal(unitConversionUtility.convertUnit(Constants.MASS, i.getWeight(), WEIGHT_UNIT_KG, tenantSettings.getWeightChargeableUnit())));
            else
                totalGrossWeightOfMawbGood = totalGrossWeightOfMawbGood.add(convertToBigDecimal(unitConversionUtility.convertUnit(Constants.MASS, i.getWeight(), i.getWeightUnit(), WEIGHT_UNIT_KG)));
        }
        return totalGrossWeightOfMawbGood;
    }

    @SuppressWarnings("java:S2209")
    private BigDecimal getTotalGrossVolumeOfMawbGood(ShipmentSettingsDetails tenantSettings, AwbPackingInfo i, BigDecimal totalGrossVolumeOfMawbGood) throws RunnerException {
        if (i.getVolume() != null) {
            if (i.getVolumeUnit() == null || i.getVolumeUnit().isEmpty())
                totalGrossVolumeOfMawbGood = totalGrossVolumeOfMawbGood.add(convertToBigDecimal(unitConversionUtility.convertUnit(Constants.VOLUME, i.getVolume(), VOLUME_UNIT_M3, tenantSettings.getVolumeChargeableUnit())));

            else
                totalGrossVolumeOfMawbGood = totalGrossVolumeOfMawbGood.add(convertToBigDecimal(unitConversionUtility.convertUnit(Constants.VOLUME, i.getVolume(), i.getVolumeUnit(), VOLUME_UNIT_M3)));
        }
        return totalGrossVolumeOfMawbGood;
    }

    private void saveHawbPacks(Map<String, List<AwbPackingInfo>> hawbPacksMap) {
        List<String> awbNumbers = hawbPacksMap.keySet().stream().toList();

        commonUtils.setInterBranchContextForHub();
        List<Awb> hawbList = awbDao.findAwbByAwbNumbers(awbNumbers);
        for (var hawb : hawbList) {
            hawb.setAwbPackingInfo(hawbPacksMap.get(hawb.getAwbNumber()));
        }
        awbDao.saveAll(hawbList);
    }

    private AwbResponse convertEntityToDto(Awb awbShipmentInfo) {
        var awbResponse = jsonHelper.convertValue(awbShipmentInfo, AwbResponse.class);
        awbResponse.setUserDisplayName(UserContext.getUser().DisplayName);
        if (awbShipmentInfo.getAwbSpecialHandlingCodesMappings() != null && !awbShipmentInfo.getAwbSpecialHandlingCodesMappings().isEmpty()) {
            awbResponse.setShcIdList(awbShipmentInfo.getAwbSpecialHandlingCodesMappings().stream()
                    .map(i -> i.getShcId())
                    .toList()
            );
        }
        return awbResponse;
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<Awb> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(awbShipmentInfo -> {
            var res = convertEntityToDto(awbShipmentInfo);
            setChargeCodeDetails(res);
            generateDefaultAwbInformation(awbShipmentInfo, res);
            try {
                generateDefaultAwbOtherInfo(res);
            } catch (Exception e) {
                throw new ValidationException(e.getMessage(), e);
            }
            String error = null;
            if (isEntityTypeMawbOrDMawb(res)) {
                ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
                Boolean fetchRatesWarning = isFetchRatesWarning(awbShipmentInfo);
                if (Boolean.TRUE.equals(shipmentSettingsDetails.getIataTactFlag()) && Boolean.TRUE.equals(fetchRatesWarning)) {
                    error = "The Port/ Carrier details are changed - You need to fetch the new TACT Rates.";
                }
            }
            try {
                res.setErrors(validateAwb(awbShipmentInfo));
            } catch (RunnerException e) {
                throw new ValidationException(e.getMessage(), e);
            }
            if (error != null)
                res.setErrors(res.getErrors() != null ? String.join("\r\n", res.getErrors(), error) : error);
            responseList.add(res);
        });
        return responseList;
    }

    private boolean isEntityTypeMawbOrDMawb(AwbResponse res) {
        return res.getAwbShipmentInfo().getEntityType().equals(Constants.MAWB) || res.getAwbShipmentInfo().getEntityType().equals(Constants.DMAWB);
    }

    private boolean isFetchRatesWarning(Awb awbShipmentInfo) {
        return awbShipmentInfo.getAwbGoodsDescriptionInfo().stream().anyMatch(x -> x.getRateCharge() != null && Boolean.TRUE.equals(x.getEnableFetchRatesWarning()));
    }

    private void setChargeCodeDetails(AwbResponse res) {
        var chargeCode = res.getAwbCargoInfo() != null ? res.getAwbCargoInfo().getChargeCode() : null;
        if (chargeCode != null) {
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
                if (entityTransferMasterLists != null && !entityTransferMasterLists.isEmpty())
                    res.setChargeDetails(entityTransferMasterLists.get(0));
            } catch (Exception ignored) {
                log.info(Constants.IGNORED_ERROR_MSG);
            }
        }
    }

    private Awb convertRequestToEntity(AwbRequest request) {
        var awb = jsonHelper.convertValue(request, Awb.class);
        if (!CommonUtils.listIsNullOrEmpty(awb.getAwbGoodsDescriptionInfo())) {
            awb.getAwbGoodsDescriptionInfo().forEach(good -> {
                if (good.getGuid() == null) good.setGuid(UUID.randomUUID());
                if (good.getEntityId() == null) good.setEntityId(request.getAwbShipmentInfo().getEntityId());
                if (good.getEntityType() == null) good.setEntityType(request.getAwbShipmentInfo().getEntityType());
            });
        }
        if (request.getShcIdList() != null) {
            List<AwbSpecialHandlingCodesMappingInfo> res = new ArrayList<>();
            for (var shcId : request.getShcIdList()) {
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
        if (request.getIsReset() == null || !request.getIsReset()) {
            List<Awb> existingAwbs = awbDao.findByConsolidationId(request.getConsolidationId());
            if (!existingAwbs.isEmpty())
                throw new RunnerException("MAWB already created for current Consolidation !");
        }

        // validate the request
        AwbUtility.validateConsolidationInfoBeforeGeneratingAwb(consolidationDetails);
        List<AwbPackingInfo> mawbPackingInfo = new ArrayList<>();
        List<Long> shipmentDetailsIdList = consolidationDetails.getShipmentsList()
                .stream()
                .map(ShipmentDetails::getId)
                .toList();
        List<Awb> awbList = awbDao.findByShipmentIdList(shipmentDetailsIdList);
        Map<Long, Awb> shipmentIdToAwbMap = awbList.stream()
                .collect(Collectors.toMap(Awb::getShipmentId, awb1 -> awb1));
        if (awbList.size() != shipmentDetailsIdList.size()) {
            throw new ValidationException(AwbConstants.GENERATE_HAWB_BEFORE_MAWB_EXCEPTION);
        }
        for (Long shipmentId : shipmentDetailsIdList) {
            Awb linkAwb = shipmentIdToAwbMap.get(shipmentId);
            if (linkAwb.getAwbPackingInfo() != null) {
                mawbPackingInfo.addAll(linkAwb.getAwbPackingInfo());
            }
        }

        V1TenantSettingsResponse tenantSettingsResponse = commonUtils.getCurrentTenantSettings();
        // generate Awb Entity
        List<AwbSpecialHandlingCodesMappingInfo> sph = getAwbSpecialHandlingCodesMappingInfos(request, consolidationDetails, tenantSettingsResponse);

        try {
            consolidationService.validateRaKcForConsol(consolidationDetails, tenantSettingsResponse);
        } catch (ValidationException ex) {
            throw new RunnerException(ex.getMessage());
        } catch (Exception e) {
            throw new RunnerException(String.format(RA_KC_VALIDATION_MESSAGE, Constants.CONSOLIDATION_CAMELCASE));
        }
        V1RetrieveResponse tenantResponse = v1Service.retrieveTenant();
        TenantModel tenantModel = jsonHelper.convertValue(tenantResponse.getEntity(), TenantModel.class);

        AwbCargoInfo awbCargoInfo = new AwbCargoInfo();
        ArrayList<String> addressIdsList = collectAddressIds(consolidationDetails);
        Map<Long, AddressDataV1> addressDataV1Map = fetchAddressData(addressIdsList);
        PackSummaryResponse packSummary = packingService.calculatePackSummary(consolidationDetails.getPackingList(), consolidationDetails.getTransportMode(), consolidationDetails.getContainerCategory(), new ShipmentMeasurementDetailsDto());
        Awb awb = Awb.builder()
                .awbNumber(consolidationDetails.getMawb())
                .awbShipmentInfo(generateMawbShipmentInfo(consolidationDetails, request, awbCargoInfo, tenantResponse, addressDataV1Map))
                .awbNotifyPartyInfo(generateMawbNotifyPartyinfo(consolidationDetails, request))
                .awbRoutingInfo(generateMawbRoutingInfo(consolidationDetails, request))
                .awbGoodsDescriptionInfo(generateMawbGoodsDescriptionInfo(consolidationDetails, request, null, packSummary))
                .awbOtherInfo(generateMawbOtherInfo(consolidationDetails, request))
                //.awbPackingInfo(awbPackingInfo)
                .consolidationId(consolidationDetails.getId())
                .awbSpecialHandlingCodesMappings(sph)
                .build();
        awb.setOciInfo(OCIInfo.builder().otherIdentityInfo(OtherIdentityInfo.builder().irIpAddress(convertIpFormat(getClientIp())).iaIpAddress(convertIpFormat(getClientIp())).build()).build());
        awb.setAwbCargoInfo(generateMawbCargoInfo(consolidationDetails, request, awbPackingInfo, awbCargoInfo, awb.getAwbGoodsDescriptionInfo(), tenantModel));
        awb.getAwbCargoInfo().setSci(consolidationDetails.getSci());
        return awb;
    }

    private List<AwbSpecialHandlingCodesMappingInfo> getAwbSpecialHandlingCodesMappingInfos(CreateAwbRequest request, ConsolidationDetails consolidationDetails, V1TenantSettingsResponse tenantSettingsResponse) {
        List<AwbSpecialHandlingCodesMappingInfo> sph = new ArrayList<>();
        if (Boolean.TRUE.equals(tenantSettingsResponse.getEnableAirMessaging()) && !Strings.isNullOrEmpty(consolidationDetails.getEfreightStatus())
                && (consolidationDetails.getEfreightStatus().equalsIgnoreCase(Constants.EAW)
                || consolidationDetails.getEfreightStatus().equalsIgnoreCase(Constants.EAP))) {
            sph.add(AwbSpecialHandlingCodesMappingInfo
                    .builder()
                    .shcId(consolidationDetails.getEfreightStatus())
                    .entityId(consolidationDetails.getId())
                    .entityType(request.getAwbType())
                    .build());
        }
        if (!Strings.isNullOrEmpty(consolidationDetails.getSecurityStatus()) && AwbConstants.SecurityStatusList.contains(consolidationDetails.getSecurityStatus())) {
            sph.add(AwbSpecialHandlingCodesMappingInfo
                    .builder()
                    .shcId(Objects.equals(consolidationDetails.getSecurityStatus(), AwbConstants.EXEMPTION_CARGO_SECURITY_STATUS) ? AwbConstants.SPX : consolidationDetails.getSecurityStatus())
                    .entityId(consolidationDetails.getId())
                    .entityType(request.getAwbType())
                    .build());
        }
        return sph;
    }

    private void updateSciFieldFromMawb(Awb awb, List<Awb> awbList) {
        boolean updateSci = false;
        if (!awbList.isEmpty()) {
            Optional<Awb> hawb = awbList.stream().filter(x -> Objects.equals(x.getAwbCargoInfo().getSci(), AwbConstants.T1)).findAny();
            if (hawb.isPresent()) {
                updateSci = true;
            }
        }
        if (!Objects.equals(awb.getPrintType(), PrintType.ORIGINAL_PRINTED) && (updateSci)) {
            awb.getAwbCargoInfo().setSci(AwbConstants.T1);
        }
    }

    private AwbShipmentInfo generateMawbShipmentInfo(ConsolidationDetails consolidationDetails, CreateAwbRequest request, AwbCargoInfo awbCargoInfo, V1RetrieveResponse tenantResponse, Map<Long, AddressDataV1> addressDataV1Map) throws RunnerException {
        AwbShipmentInfo awbShipmentInfo = new AwbShipmentInfo();
        TenantModel tenantModel = null;
        try {
            tenantModel = jsonHelper.convertValue(tenantResponse.getEntity(), TenantModel.class);
        } catch (Exception e) {
            throw new RunnerException("Failed while fetching tenant info from V1");
        }
        Map<String, String> alpha2DigitToCountry = masterDataUtils.consolidationAddressCountryMasterData(consolidationDetails);
        awbShipmentInfo.setEntityId(consolidationDetails.getId());
        awbShipmentInfo.setEntityType(request.getAwbType());
        awbShipmentInfo.setAwbNumber(consolidationDetails.getMawb());
        awbShipmentInfo.setFirstCarrier(consolidationDetails.getCarrierDetails().getShippingLine());
        awbShipmentInfo.setShipperReferenceNumber(consolidationDetails.getAgentReference());

        processConsoleSendingAgentAndReceivingAgent(consolidationDetails, awbShipmentInfo, alpha2DigitToCountry, addressDataV1Map);
        setAwbShipmentInfoUnLocationData(awbShipmentInfo, consolidationDetails.getCarrierDetails());
        setTenantFieldsInAwbShipmentInfo(awbShipmentInfo, tenantModel);

        for (var orgRow : consolidationDetails.getConsolidationAddresses()) {
            if (orgRow.getType().equals(Constants.FAG)) {
                ArrayList<String> issuingAgentAddressList = new ArrayList<>();
                issuingAgentAddressList.add(orgRow.getAddressId());
                Map<Long, AddressDataV1> issuingAgentAddressDataMap = fetchAddressData(issuingAgentAddressList);
                AddressDataV1 issuingAgentAddressData = null;
                if(orgRow.getAddressId() != null){
                    String addressId = orgRow.getAddressId();
                    issuingAgentAddressData = issuingAgentAddressDataMap.get(Long.valueOf(addressId));
                }
                var issuingAgentName = StringUtility.convertToString(orgRow.getOrgData().get(PartiesConstants.FULLNAME));
                awbShipmentInfo.setIssuingAgentName(issuingAgentName == null ? issuingAgentName : issuingAgentName.toUpperCase());
                constructIssuingAgentAddress(awbShipmentInfo, orgRow.getAddressData(), alpha2DigitToCountry, issuingAgentAddressData);
                setAwbCustomOriginCode(awbCargoInfo, orgRow);
                setIataAndCassCode(orgRow, awbShipmentInfo);
            }
        }

        if (awbShipmentInfo.getIssuingAgentName() == null || awbShipmentInfo.getIssuingAgentName().isEmpty()) {
            populateIssuingAgent(awbShipmentInfo, tenantModel, awbCargoInfo);
        }

        return awbShipmentInfo;
    }

    private void setIataAndCassCode(Parties orgRow, AwbShipmentInfo awbShipmentInfo) {
        awbShipmentInfo.setIataCode(StringUtility.isEmpty(awbShipmentInfo.getIataCode())
                ? StringUtility.convertToString(orgRow.getOrgData().get(PartiesConstants.AGENT_IATA_CODE))
                : awbShipmentInfo.getIataCode());
        awbShipmentInfo.setAgentCASSCode(StringUtility.isEmpty(awbShipmentInfo.getAgentCASSCode())
                ? StringUtility.convertToString(orgRow.getOrgData().get(PartiesConstants.AGENT_IATA_CODE))
                : awbShipmentInfo.getAgentCASSCode());
    }

    private void setAwbIssuingAgentTaxRegistrationNumber(Parties orgRow, Map<Long, AddressDataV1> issuingAgentAddressIdToEntityMap, AwbShipmentInfo awbShipmentInfo) {
        if (orgRow.getAddressId() != null && !orgRow.getAddressId().isEmpty() && issuingAgentAddressIdToEntityMap.containsKey(Long.valueOf(orgRow.getAddressId()))) {
            awbShipmentInfo.setIssuingAgentTaxRegistrationNumber(issuingAgentAddressIdToEntityMap.get(Long.valueOf(orgRow.getAddressId())).getTaxRegNumber() != null ? StringUtility.toUpperCase(StringUtility.convertToString(issuingAgentAddressIdToEntityMap.get(Long.valueOf(orgRow.getAddressId())).getTaxRegNumber())) : null);
        }
    }

    private void setAwbCustomOriginCode(AwbCargoInfo awbCargoInfo, Parties orgRow) {
        String country = orgRow.getOrgData() != null ?
                (String) orgRow.getOrgData().get(COUNTRY) : null;
        if (country != null)
            awbCargoInfo.setCustomOriginCode(getCountryCode(country));
    }

    private void processConsoleSendingAgentAndReceivingAgent(ConsolidationDetails consolidationDetails, AwbShipmentInfo awbShipmentInfo, Map<String, String> alpha2DigitToCountry, Map<Long, AddressDataV1> addressDataV1Map) {
        AddressDataV1 sendingAgentAddressData = null;
        if(consolidationDetails.getSendingAgent() != null && consolidationDetails.getSendingAgent().getAddressId() != null){
            String sendingAddressId = consolidationDetails.getSendingAgent().getAddressId();
            sendingAgentAddressData = addressDataV1Map.get(Long.valueOf(sendingAddressId));
        }
        constructShipperAddress(awbShipmentInfo, consolidationDetails.getSendingAgent() != null ? consolidationDetails.getSendingAgent().getAddressData() : null, alpha2DigitToCountry, sendingAgentAddressData);
        var shipperName = StringUtility.convertToString(consolidationDetails.getSendingAgent() != null && consolidationDetails.getReceivingAgent().getOrgData() != null ? consolidationDetails.getSendingAgent().getOrgData().get(PartiesConstants.FULLNAME) : "");
        awbShipmentInfo.setShipperName(shipperName == null ? shipperName : shipperName.toUpperCase());
        AddressDataV1 consigneeAddressData = null;
        if(consolidationDetails.getReceivingAgent() != null && consolidationDetails.getReceivingAgent().getAddressId() != null){
            String consigneeAddressId = consolidationDetails.getReceivingAgent().getAddressId();
            consigneeAddressData = addressDataV1Map.get(Long.valueOf(consigneeAddressId));
        }
        var consigneeName = StringUtility.convertToString(consolidationDetails.getReceivingAgent() != null && consolidationDetails.getReceivingAgent().getOrgData() != null ? consolidationDetails.getReceivingAgent().getOrgData().get(PartiesConstants.FULLNAME) : "");
        awbShipmentInfo.setConsigneeName(consigneeName == null ? consigneeName : consigneeName.toUpperCase());
        awbShipmentInfo.setConsigneeReferenceNumber(consolidationDetails.getReceivingAgent() != null ? consolidationDetails.getReceivingAgent().getId().toString() : null);
        constructConsigneeAddress(awbShipmentInfo, consolidationDetails.getReceivingAgent() != null ? consolidationDetails.getReceivingAgent().getAddressData() : null, alpha2DigitToCountry, consigneeAddressData);
    }

    private void constructShipperAddress(AwbShipmentInfo awbShipmentInfo, Map<String, Object> address, Map<String, String> alpha2ToCountryMap, AddressDataV1 shipperAddressData) {
        if (shipperAddressData!=null && address!=null) {
            awbShipmentInfo.setShipperAddress(StringUtility.convertToString(shipperAddressData.getAddress1()!=null ? shipperAddressData.getAddress1().toUpperCase() : ""));
            awbShipmentInfo.setShipperAddress2(StringUtility.convertToString(shipperAddressData.getAddress2()!=null ? shipperAddressData.getAddress2().toUpperCase() : ""));
            awbShipmentInfo.setShipperCity(StringUtility.convertToString(shipperAddressData.getCity()!=null ? shipperAddressData.getCity() : ""));
            if (address.containsKey(PartiesConstants.COUNTRY))
                constructShipperCountry(awbShipmentInfo, address, alpha2ToCountryMap);
            awbShipmentInfo.setShipperState(StringUtility.convertToString(shipperAddressData.getState()!=null ? shipperAddressData.getState() : ""));
            awbShipmentInfo.setShipperZipCode(StringUtility.convertToString(shipperAddressData.getZipPostCode()!=null ? shipperAddressData.getZipPostCode() : ""));
            awbShipmentInfo.setShipperContactName(StringUtility.toUpperCase(StringUtility.convertToString(shipperAddressData.getContactPerson()!=null ? shipperAddressData.getContactPerson() : "")));
            awbShipmentInfo.setShipperPhone(StringUtility.convertToString(shipperAddressData.getContactPhone()!=null ? shipperAddressData.getContactPhone() : ""));
            awbShipmentInfo.setIssuingAgentTaxRegistrationNumber(StringUtility.convertToString(shipperAddressData.getTaxRegNumber()!=null ? shipperAddressData.getTaxRegNumber() : ""));
        }
    }

    private void constructShipperCountry(AwbShipmentInfo awbShipmentInfo, Map<String, Object> address, Map<String, String> alpha2ToCountryMap) {
        String country = StringUtility.convertToString(address.get(PartiesConstants.COUNTRY));
        if (country != null && country.length() == 3)
            country = CountryListHelper.ISO3166.getAlpha2FromAlpha3(country);
        if (alpha2ToCountryMap != null && alpha2ToCountryMap.containsKey(country))
            awbShipmentInfo.setShipperCountryName(alpha2ToCountryMap.get(country));
        awbShipmentInfo.setShipperCountry(country);
    }

    private void constructConsigneeAddress(AwbShipmentInfo awbShipmentInfo, Map<String, Object> address, Map<String, String> alpha2ToCountryMap, AddressDataV1 consigneeAddressData) {
        if (address != null && consigneeAddressData != null) {
            awbShipmentInfo.setConsigneeAddress(StringUtility.convertToString(consigneeAddressData.getAddress1() != null ? consigneeAddressData.getAddress1().toUpperCase() : ""));
            awbShipmentInfo.setConsigneeAddress2(StringUtility.convertToString(consigneeAddressData.getAddress2() != null ? consigneeAddressData.getAddress2().toUpperCase() : ""));
            awbShipmentInfo.setConsigneeCity(StringUtility.convertToString(consigneeAddressData.getCity() != null ? consigneeAddressData.getCity() : ""));
            if (address.containsKey(PartiesConstants.COUNTRY)) {
                constructConsigneeCountry(awbShipmentInfo, address, alpha2ToCountryMap);
            }
            awbShipmentInfo.setConsigneeState(StringUtility.convertToString(consigneeAddressData.getState() != null ? consigneeAddressData.getState() : ""));
            awbShipmentInfo.setConsigneeZipCode(StringUtility.convertToString(consigneeAddressData.getZipPostCode() != null ? consigneeAddressData.getZipPostCode() : ""));
            awbShipmentInfo.setConsigneeContactName(StringUtility.toUpperCase(StringUtility.convertToString(consigneeAddressData.getContactPerson() != null ? consigneeAddressData.getContactPerson() : "")));
            awbShipmentInfo.setConsigneePhone(StringUtility.convertToString(consigneeAddressData.getContactPhone() != null ? consigneeAddressData.getContactPhone() : ""));
            awbShipmentInfo.setConsigneeTaxRegistrationNumber(StringUtility.convertToString(consigneeAddressData.getTaxRegNumber() != null ? consigneeAddressData.getTaxRegNumber() : ""));
        }
    }

    private void constructConsigneeCountry(AwbShipmentInfo awbShipmentInfo, Map<String, Object> address, Map<String, String> alpha2ToCountryMap) {
        String country = StringUtility.convertToString(address.get(PartiesConstants.COUNTRY));
        if (country != null && country.length() == 3)
            country = CountryListHelper.ISO3166.getAlpha2FromAlpha3(country);
        if (alpha2ToCountryMap != null && alpha2ToCountryMap.containsKey(country))
            awbShipmentInfo.setConsigneeCountryName(alpha2ToCountryMap.get(country));
        awbShipmentInfo.setConsigneeCountry(country);
    }

    private void constructIssuingAgentAddress(AwbShipmentInfo awbShipmentInfo, Map<String, Object> address, Map<String, String> alpha2ToCountryMap, AddressDataV1 issuingAgentAddressData) {
        if (address != null && issuingAgentAddressData!=null) {
            awbShipmentInfo.setIssuingAgentAddress(StringUtility.convertToString(issuingAgentAddressData.getAddress1() != null ? issuingAgentAddressData.getAddress1().toUpperCase() : ""));
            awbShipmentInfo.setIssuingAgentAddress2(StringUtility.convertToString(issuingAgentAddressData.getAddress2() != null ? issuingAgentAddressData.getAddress2().toUpperCase() : ""));
            awbShipmentInfo.setIssuingAgentCity(StringUtility.convertToString(issuingAgentAddressData.getCity() != null ? issuingAgentAddressData.getCity() : ""));
            if (address.containsKey(PartiesConstants.COUNTRY))
                constrcutIssuingAgentCountry(awbShipmentInfo, address, alpha2ToCountryMap);
            awbShipmentInfo.setIssuingAgentState(StringUtility.convertToString(issuingAgentAddressData.getState() != null ? issuingAgentAddressData.getState() : ""));
            awbShipmentInfo.setIssuingAgentZipCode(StringUtility.convertToString(issuingAgentAddressData.getZipPostCode() != null ? issuingAgentAddressData.getZipPostCode() : ""));
            awbShipmentInfo.setIssuingAgentContactName(StringUtility.toUpperCase(StringUtility.convertToString(issuingAgentAddressData.getContactPerson() != null ? issuingAgentAddressData.getContactPerson() : "")));
            awbShipmentInfo.setIssuingAgentPhone(StringUtility.convertToString(issuingAgentAddressData.getContactPhone() != null ? issuingAgentAddressData.getContactPhone() : ""));
            awbShipmentInfo.setIssuingAgentTaxRegistrationNumber(StringUtility.convertToString(issuingAgentAddressData.getTaxRegNumber() != null ? issuingAgentAddressData.getTaxRegNumber() : ""));
        }
    }

    private void constrcutIssuingAgentCountry(AwbShipmentInfo awbShipmentInfo, Map<String, Object> address, Map<String, String> alpha2ToCountryMap) {
        String country = StringUtility.convertToString(address.get(PartiesConstants.COUNTRY));
        if (country != null && country.length() == 3)
            country = CountryListHelper.ISO3166.getAlpha2FromAlpha3(country);
        if (alpha2ToCountryMap != null && alpha2ToCountryMap.containsKey(country))
            awbShipmentInfo.setIssuingAgentCountryName(alpha2ToCountryMap.get(country));
        awbShipmentInfo.setIssuingAgentCountry(country);
    }

    private void constructNotifyPartyAddress(AwbNotifyPartyInfo notifyPartyInfo, Map<String, Object> address, Map<String, String> alpha2ToCountryMap, AddressDataV1 addressData) {
        if (address != null && addressData != null) {
            notifyPartyInfo.setAddress(StringUtility.convertToString(addressData.getAddress1() != null ? addressData.getAddress1().toUpperCase() : ""));
            notifyPartyInfo.setAddress2(StringUtility.convertToString(addressData.getAddress2() != null ? addressData.getAddress2().toUpperCase() : ""));
            notifyPartyInfo.setCity(StringUtility.convertToString(addressData.getCity() != null ? addressData.getCity() : ""));
            if (address.containsKey(PartiesConstants.COUNTRY)) {
                constructNotifyPartyCountry(notifyPartyInfo, address, alpha2ToCountryMap);
            }
            notifyPartyInfo.setState(StringUtility.convertToString(addressData.getState() != null ? addressData.getState() : ""));
            notifyPartyInfo.setZipCode(StringUtility.convertToString(addressData.getZipPostCode() != null ? addressData.getZipPostCode() : ""));
            notifyPartyInfo.setContactName(StringUtility.toUpperCase(StringUtility.convertToString(addressData.getContactPerson() != null ? addressData.getContactPerson() : "")));
            notifyPartyInfo.setPhone(StringUtility.convertToString(addressData.getContactPhone() != null ? addressData.getContactPhone() : ""));
            notifyPartyInfo.setTaxRegistrationNumber(StringUtility.convertToString(addressData != null ? addressData.getTaxRegNumber(): ""));
        }
    }

    private void constructNotifyPartyCountry(AwbNotifyPartyInfo notifyPartyInfo, Map<String, Object> address, Map<String, String> alpha2ToCountryMap) {
        String country = StringUtility.convertToString(address.get(PartiesConstants.COUNTRY));
        if (country != null && country.length() == 3)
            country = CountryListHelper.ISO3166.getAlpha2FromAlpha3(country);
        if (alpha2ToCountryMap != null && alpha2ToCountryMap.containsKey(country))
            notifyPartyInfo.setCountryName(alpha2ToCountryMap.get(country));
        notifyPartyInfo.setCountry(country);
    }

    private List<AwbNotifyPartyInfo> generateMawbNotifyPartyinfo(ConsolidationDetails consolidationDetails, CreateAwbRequest request) {
        if (consolidationDetails.getConsolidationAddresses() != null &&
                !consolidationDetails.getConsolidationAddresses().isEmpty()) {
            Map<String, String> alpha2DigitToCountry = masterDataUtils.consolidationAddressCountryMasterData(consolidationDetails);
            List<AwbNotifyPartyInfo> notifyPartyList = new ArrayList<>();
            ArrayList<String> notifyAddressIdList = new ArrayList<>();
            notifyAddressIdList.addAll(consolidationDetails.getConsolidationAddresses()
                    .stream()
                    .filter(party -> party.getType() != null && (party.getType().equals("Notify Part 1") || party.getType().equals("Notify Part 2") || party.getType().equals("Notify Part 3")))
                    .filter(parties -> parties.getAddressId() !=null)
                    .map(Parties::getAddressId)
                    .collect(Collectors.toList()));
            Map<Long, AddressDataV1> notifyPartyAddressDataMap = fetchAddressData(notifyAddressIdList);
            for (var party : consolidationDetails.getConsolidationAddresses()) {
                processConsolidationAddress(consolidationDetails, request, party, alpha2DigitToCountry, notifyPartyList, notifyAddressIdList, notifyPartyAddressDataMap);
            }
            for (int i = 0; i < notifyPartyList.size(); i++) {
                AwbNotifyPartyInfo notifyParty = notifyPartyList.get(i);
                setTaxRegistrationNumber(notifyParty, notifyPartyAddressDataMap);
            }
            return notifyPartyList;
        }
        return null;
    }

    private void setTaxRegistrationNumber(AwbNotifyPartyInfo notifyParty, Map<Long, AddressDataV1> notifyPartyAddressIdToEntityOrgMap) {
        if (notifyParty.getAddressId() != null && notifyPartyAddressIdToEntityOrgMap.containsKey(Long.valueOf(notifyParty.getAddressId())) && notifyPartyAddressIdToEntityOrgMap.get(Long.valueOf(notifyParty.getAddressId())).getTaxRegNumber() != null) {
            notifyParty.setTaxRegistrationNumber(StringUtility.toUpperCase(StringUtility.convertToString(notifyPartyAddressIdToEntityOrgMap.get(Long.valueOf(notifyParty.getAddressId())).getTaxRegNumber())));
        }
    }

    private void processConsolidationAddress(ConsolidationDetails consolidationDetails, CreateAwbRequest request, Parties party, Map<String, String> alpha2DigitToCountry, List<AwbNotifyPartyInfo> notifyPartyList, ArrayList<String> notifyAddressIdList, Map<Long, AddressDataV1> notifyPartyAddressDataMap) {
        if (party.getType() != null && (party.getType().equals("Notify Part 1") || party.getType().equals("Notify Part 2") || party.getType().equals("Notify Part 3"))) {
            String partyAddressId = party.getAddressId();
            AddressDataV1 addressData = notifyPartyAddressDataMap.get(partyAddressId);
            AwbNotifyPartyInfo notifyPartyInfo = new AwbNotifyPartyInfo();
            var name = StringUtility.convertToString(party.getOrgData().get(PartiesConstants.FULLNAME));
            notifyPartyInfo.setName(name == null ? name : name.toUpperCase());
            notifyPartyInfo.setEntityId(consolidationDetails.getId());
            notifyPartyInfo.setEntityType(request.getAwbType());
            notifyPartyInfo.setGuid(party.getGuid());
            notifyPartyInfo.setIsShipmentCreated(true);
            constructNotifyPartyAddress(notifyPartyInfo, party.getAddressData(), alpha2DigitToCountry, addressData);
            // org and address data
            var orgId = party.getOrgData() != null ? (Integer) party.getOrgData().get("Id") : null;
            var addressId = party.getAddressData() != null ? Integer.valueOf((String) party.getAddressData().get("Id")) : null;
            notifyPartyInfo.setOrgId(orgId);
            if (party.getAddressId() != null && !party.getAddressData().isEmpty()) {
                notifyPartyInfo.setAddressId(Integer.valueOf(party.getAddressId()));
            }
            notifyPartyInfo.setAddressId(addressId);
            notifyPartyInfo.setNotifyOrgId(Long.valueOf(orgId));
            notifyPartyList.add(notifyPartyInfo);
            notifyAddressIdList.add(party.getAddressId());
        }
    }

    private List<AwbRoutingInfo> generateMawbRoutingInfo(ConsolidationDetails consolidationDetails, CreateAwbRequest request) {
        if (consolidationDetails.getRoutingsList() != null && !consolidationDetails.getRoutingsList().isEmpty()) {
            var sortedRoutingList = consolidationDetails.getRoutingsList().stream().filter(route -> Objects.equals(route.getMode(), Constants.TRANSPORT_MODE_AIR) && Objects.equals(route.getCarriage(), RoutingCarriage.MAIN_CARRIAGE)).collect(Collectors.toList());
            if (sortedRoutingList != null && !sortedRoutingList.isEmpty()) {
                List<AwbRoutingInfo> res = new ArrayList<>();
                Collections.sort(sortedRoutingList, Comparator.comparing(Routings::getLeg));
                Long leg = 1L;
                for (var route : sortedRoutingList) {
                    AwbRoutingInfo awbRoutingInfo = new AwbRoutingInfo();
                    awbRoutingInfo.setIsShipmentCreated(true);
                    awbRoutingInfo.setOriginPortName(route.getPol());
                    awbRoutingInfo.setDestinationPortName(route.getPod());
                    awbRoutingInfo.setByCarrier(route.getCarrier());
                    awbRoutingInfo.setFlightNumber(Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getEnableRouteMaster()) ? route.getVoyage() : route.getFlightNumber());
                    awbRoutingInfo.setFlightDate(route.getEtd());
                    awbRoutingInfo.setEta(route.getEta());
                    awbRoutingInfo.setEntityId(consolidationDetails.getId());
                    awbRoutingInfo.setEntityType(request.getAwbType());
                    awbRoutingInfo.setLeg(leg++);
                    res.add(awbRoutingInfo);
                }
                return res;
            }
        } else if (consolidationDetails.getCarrierDetails() != null &&
                consolidationDetails.getCarrierDetails().getOriginPort() != null &&
                consolidationDetails.getCarrierDetails().getDestinationPort() != null
        ) {
            AwbRoutingInfo routingInfo = new AwbRoutingInfo();
            routingInfo.setIsShipmentCreated(true);
            routingInfo.setFlightDate(consolidationDetails.getCarrierDetails().getEtd());
            routingInfo.setEta(consolidationDetails.getCarrierDetails().getEta());
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

    private AwbCargoInfo generateMawbCargoInfo(ConsolidationDetails consolidationDetails, CreateAwbRequest request, List<AwbPackingInfo> awbPackingList, AwbCargoInfo awbCargoInfo, List<AwbGoodsDescriptionInfo> awbGoodsDescriptionInfos, TenantModel tenantModel) {
        if (awbCargoInfo == null) {
            awbCargoInfo = new AwbCargoInfo();
        }

        GenerateAwbPaymentInfoRequest generateAwbPaymentInfoRequest = new GenerateAwbPaymentInfoRequest();
        generateAwbPaymentInfoRequest.setAwbCargoInfo(awbCargoInfo);
        generateAwbPaymentInfoRequest.setAwbPackingInfo(awbPackingList);
        generateAwbPaymentInfoRequest.setAwbGoodsDescriptionInfo(awbGoodsDescriptionInfos);
        generateAwbPaymentInfoRequest.setIsFromShipment(false);
        generateAwbPaymentInfoRequest.setPackUpdate(false);
        awbCargoInfo.setEntityId(consolidationDetails.getId());
        awbCargoInfo.setEntityType(request.getAwbType());
//        awbCargoInfo.setCarriageValue(shipmentDetails.getGoodsValue() != null ? shipmentDetails.getGoodsValue() : new BigDecimal(0.0)); // field missing
//        awbCargoInfo.setCarriageValue(shipmentDetails.getInsuranceValue() != null ? shipmentDetailsgetInsuranceValue() : new BigDecimal(0.0)); // field missing
        awbCargoInfo.setCustomsValue(BigDecimal.valueOf(0.0));
        awbCargoInfo.setCurrency(UserContext.getUser().getCompanyCurrency());
        awbCargoInfo.setHandlingInfo(getHandlingInfo(MasterDataType.MAWB_GENERATION, awbPackingList, consolidationDetails.getHazardous()));
        awbCargoInfo.setAccountingInfo(awbCargoInfo.getAccountingInfo() == null ? null : awbCargoInfo.getAccountingInfo().toUpperCase());
        awbCargoInfo.setOtherInfo(awbCargoInfo.getOtherInfo() == null ? null : awbCargoInfo.getOtherInfo().toUpperCase());
        awbCargoInfo.setShippingInformation(awbCargoInfo.getShippingInformation() == null ? null : awbCargoInfo.getShippingInformation().toUpperCase());
        awbCargoInfo.setShippingInformationOther(awbCargoInfo.getShippingInformationOther() == null ? null : awbCargoInfo.getShippingInformationOther().toUpperCase());
        if(consolidationDetails.getIncoterms() != null && AwbConstants.incotermChargeCodeMap.containsKey(consolidationDetails.getIncoterms()))
            awbCargoInfo.setChargeCode(AwbConstants.incotermChargeCodeMap.get(consolidationDetails.getIncoterms()));
        populateCsdInfo(awbCargoInfo, tenantModel);
        // Set Screening Status, Other info (in case of AOM), security Status, screening status, exemption code
        awbCargoInfo.setScreeningStatus(consolidationDetails.getScreeningStatus());
        awbCargoInfo.setOtherMethod(consolidationDetails.getAomFreeText());
        awbCargoInfo.setSecurityStatus(consolidationDetails.getSecurityStatus());
        awbCargoInfo.setExemptionCode(consolidationDetails.getExemptionCodes());
        return awbCargoInfo;
    }

    private List<AwbGoodsDescriptionInfo> generateMawbGoodsDescriptionInfo(ConsolidationDetails consolidationDetails, CreateAwbRequest request, List<AwbPackingInfo> awbPackingList, PackSummaryResponse packSummary) {
        String defaultTextForQuantAndGoods = Constants.DEFAULT_NATURE_AND_QUANTITY_GOODS_TEXT_MAWB;
        AwbGoodsDescriptionInfo awbGoodsDescriptionInfo = new AwbGoodsDescriptionInfo();
        awbGoodsDescriptionInfo.setEntityId(consolidationDetails.getId());
        awbGoodsDescriptionInfo.setEntityType(request.getAwbType());
        awbGoodsDescriptionInfo.setGrossWtUnit("KG");
        awbGoodsDescriptionInfo.setIsShipmentCreated(true);
        awbGoodsDescriptionInfo.setGuid(UUID.randomUUID());
        awbGoodsDescriptionInfo.setNtrQtyGoods(defaultTextForQuantAndGoods);
        awbGoodsDescriptionInfo.setGrossVolume(packSummary.getPacksVolume() != null ? packSummary.getPacksVolume().setScale(3, RoundingMode.HALF_UP) : BigDecimal.ZERO.setScale(3, RoundingMode.HALF_UP));
        awbGoodsDescriptionInfo.setGrossVolumeUnit("M3");
        if (awbPackingList != null) {
            for (var awbPacking : awbPackingList) {
                awbPacking.setAwbGoodsDescriptionInfoGuid(awbGoodsDescriptionInfo.getGuid());
            }
            awbGoodsDescriptionInfo.setAwbPackingInfo(awbPackingList);
        }
        return Arrays.asList(awbGoodsDescriptionInfo);
    }

    private String getTenantBranch() {
        String branch = UserContext.getUser().getTenantDisplayName();
        return branch != null ? branch : "";
    }

    private String getLegalCompanyName() {
        V1TenantSettingsResponse tenantSettings = commonUtils.getCurrentTenantSettings();
        return tenantSettings != null && tenantSettings.getLegalEntityCode() != null
                ? tenantSettings.getLegalEntityCode()
                : "";
    }

    private CompanyDto fetchCompanyDetails() {
        Integer companyId = UserContext.getUser().getCompanyId();
        List<Object> criteria = new ArrayList<>(List.of(List.of("Id"), "=", companyId));
        CommonV1ListRequest request = CommonV1ListRequest.builder().criteriaRequests(criteria).build();
        V1DataResponse response = v1Service.getCompaniesDetails(request);
        List<CompanyDto> companyList = response != null ? jsonHelper.convertValueToList(response.getEntities(), CompanyDto.class) : null;
        return companyList != null && !companyList.isEmpty() ? companyList.get(0) : null;
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

        String firstCarrier = consolidationDetails.getCarrierDetails() != null ? consolidationDetails.getCarrierDetails().getShippingLine() : null;
        awbOtherInfo.setCarrierName(firstCarrier);
        populateCarrierDetails(firstCarrier, awbOtherInfo, AwbOtherInfo::setCarrierHqAddress);
        return awbOtherInfo;
    }

    private <T> void populateCarrierDetails(String carrier, T awbObject, BiConsumer<T, String> setCarrierHqAddress) {

        if (!Strings.isNullOrEmpty(carrier)) {
            var masterData = masterDataUtils.fetchInBulkCarriers(Set.of(carrier));
            if (!Objects.isNull(masterData) && masterData.containsKey(carrier)) {
                String hqAddress = masterData.get(carrier).getHeadQuartersDetails();
                setCarrierHqAddress.accept(awbObject, hqAddress != null ? hqAddress : "");
            }
        }
    }

    private void linkHawbMawb(Awb mawb, List<Awb> awbList, Boolean isInterBranchConsole) throws RunnerException {
        if (Boolean.TRUE.equals(isInterBranchConsole))
            commonUtils.setInterBranchContextForHub();
        for (var awb : awbList) {
            if (awb.getAwbPackingInfo() != null) {
                for (AwbPackingInfo awbPackingInfo : awb.getAwbPackingInfo()) {
                    awbPackingInfo.setMawbGoodsDescGuid(mawb.getAwbGoodsDescriptionInfo() == null || mawb.getAwbGoodsDescriptionInfo().isEmpty() ? null : mawb.getAwbGoodsDescriptionInfo().get(0).getGuid());
                }
            }
            awbDao.save(awb);

            MawbHawbLink mawbHawblink = new MawbHawbLink();
            mawbHawblink.setHawbId(awb.getId());
            mawbHawblink.setMawbId(mawb.getId());
            mawbHawbLinkDao.save(mawbHawblink);
        }
    }

    private Awb generateAwb(CreateAwbRequest request) throws RunnerException {

        if (request.getIsReset() == null || !request.getIsReset()) {
            List<Awb> existingAwbs = awbDao.findByShipmentId(request.getShipmentId());
            if (!existingAwbs.isEmpty())
                throw new RunnerException("AWB already created for current Shipment !");
        }

        // fetch sehipment info
        ShipmentDetails shipmentDetails = shipmentDao.findById(request.getShipmentId()).get();
        boolean syncShipment = isSyncShipment(shipmentDetails);

        // fetch all packings
        List<Packing> packings = shipmentDetails.getPackingList();

        // validate the request
        AwbUtility.validateShipmentInfoBeforeGeneratingAwb(shipmentDetails);

        var awbPackingInfo = generateAwbPackingInfo(shipmentDetails, packings);
        if (syncShipment) {
            try {
                shipmentSync.sync(shipmentDetails, null, null, UUID.randomUUID().toString(), false);
            } catch (Exception e) {
                log.error("Error performing sync on shipment entity, {}", e);
            }
        }
        V1TenantSettingsResponse tenantSettingsResponse = commonUtils.getCurrentTenantSettings();
        List<AwbSpecialHandlingCodesMappingInfo> sph = getAwbSPHInfos(request, tenantSettingsResponse, shipmentDetails);

        try {
            shipmentService.validateRaKcDetails(shipmentDetails, tenantSettingsResponse);
        } catch (ValidationException ex) {
            throw new RunnerException(ex.getMessage());
        } catch (Exception ex) {
            throw new RunnerException(String.format(RA_KC_VALIDATION_MESSAGE, "Shipments"));
        }

        AwbCargoInfo awbCargoInfo = new AwbCargoInfo();
        V1RetrieveResponse tenantResponse = v1Service.retrieveTenant();
        TenantModel tenantModel = jsonHelper.convertValue(tenantResponse.getEntity(), TenantModel.class);
        ArrayList<String> addressIds = collectAddressIds(shipmentDetails);
        Map<Long, AddressDataV1> addressIdToAddressEntityMap = fetchAddressData(addressIds);
        // generate Awb Entity
        Awb awb = Awb.builder()
                .awbNumber(request.getAwbType().equals(Constants.DMAWB) ? shipmentDetails.getMasterBill() : shipmentDetails.getHouseBill())
                .awbShipmentInfo(generateAwbShipmentInfo(shipmentDetails, request, awbCargoInfo, tenantResponse, addressIdToAddressEntityMap))
                .awbNotifyPartyInfo(generateAwbNotifyPartyinfo(shipmentDetails, request, addressIdToAddressEntityMap))
                .awbRoutingInfo(generateAwbRoutingInfo(shipmentDetails, request))
                .awbGoodsDescriptionInfo(generateAwbGoodsDescriptionInfo(shipmentDetails, request, awbPackingInfo))
                .awbOtherInfo(generateAwbOtherInfo(shipmentDetails, request))
                .awbPackingInfo(awbPackingInfo)
                .shipmentId(shipmentDetails.getId())
                .awbSpecialHandlingCodesMappings(sph)
                .build();
        awb.setAwbCargoInfo(generateAwbCargoInfo(shipmentDetails, request, awbPackingInfo, awbCargoInfo, awb.getAwbGoodsDescriptionInfo(), tenantModel));
        awb.getAwbCargoInfo().setSci(shipmentDetails.getAdditionalDetails().getSci());
        return awb;
    }

    private List<AwbSpecialHandlingCodesMappingInfo> getAwbSPHInfos(CreateAwbRequest request, V1TenantSettingsResponse tenantSettingsResponse, ShipmentDetails shipmentDetails) {
        List<AwbSpecialHandlingCodesMappingInfo> sph = new ArrayList<>();
        if (Boolean.TRUE.equals(tenantSettingsResponse.getEnableAirMessaging()) && !Strings.isNullOrEmpty(shipmentDetails.getAdditionalDetails().getEfreightStatus())
                && (shipmentDetails.getAdditionalDetails().getEfreightStatus().equalsIgnoreCase(Constants.EAW) ||
                (Objects.equals(shipmentDetails.getJobType(), Constants.SHIPMENT_TYPE_DRT) && shipmentDetails.getAdditionalDetails().getEfreightStatus().equalsIgnoreCase(Constants.EAP)))) {
            sph.add(AwbSpecialHandlingCodesMappingInfo
                    .builder()
                    .shcId(shipmentDetails.getAdditionalDetails().getEfreightStatus())
                    .entityId(shipmentDetails.getId())
                    .entityType(request.getAwbType())
                    .build());
        }
        if (!Strings.isNullOrEmpty(shipmentDetails.getSecurityStatus()) && AwbConstants.SecurityStatusList.contains(shipmentDetails.getSecurityStatus())) {
            sph.add(AwbSpecialHandlingCodesMappingInfo
                    .builder()
                    .shcId(Objects.equals(shipmentDetails.getSecurityStatus(), AwbConstants.EXEMPTION_CARGO_SECURITY_STATUS) ? AwbConstants.SPX : shipmentDetails.getSecurityStatus())
                    .entityId(shipmentDetails.getId())
                    .entityType(request.getAwbType())
                    .build());
        }
        return sph;
    }

    private boolean isSyncShipment(ShipmentDetails shipmentDetails) throws RunnerException {
        boolean syncShipment = false;
        if (StringUtility.isEmpty(shipmentDetails.getHouseBill())) {
            if (!(Objects.equals(Constants.SHIPMENT_TYPE_DRT, shipmentDetails.getJobType()) && Objects.equals(Constants.TRANSPORT_MODE_AIR, shipmentDetails.getTransportMode()))) {
                shipmentDetails.setHouseBill(shipmentService.generateCustomHouseBL(shipmentDetails));
            }
            shipmentDao.save(shipmentDetails, false);
            syncShipment = true;
        }
        return syncShipment;
    }

    private AwbShipmentInfo generateAwbShipmentInfo(ShipmentDetails shipmentDetails, CreateAwbRequest request, AwbCargoInfo awbCargoInfo, V1RetrieveResponse tenantResponse, Map<Long, AddressDataV1> addressDataV1Map) throws RunnerException {
        AwbShipmentInfo awbShipmentInfo = new AwbShipmentInfo();
        Map<String, String> alpha2DigitToCountry = masterDataUtils.shipmentAddressCountryMasterData(shipmentDetails);
        awbShipmentInfo.setEntityId(shipmentDetails.getId());
        awbShipmentInfo.setEntityType(request.getAwbType());
        awbShipmentInfo.setAwbNumber(request.getAwbType().equals(Constants.DMAWB) ? shipmentDetails.getMasterBill() : shipmentDetails.getHouseBill());
        processShipmentConsignerConsginee(shipmentDetails, awbShipmentInfo, alpha2DigitToCountry, addressDataV1Map);

        setAwbShipmentInfoUnLocationData(awbShipmentInfo, shipmentDetails.getCarrierDetails());
        awbShipmentInfo.setFirstCarrier(shipmentDetails.getCarrierDetails() != null ? shipmentDetails.getCarrierDetails().getShippingLine() : null);
        for (var orgRow : shipmentDetails.getShipmentAddresses()) {
            if (orgRow.getType().equals(Constants.FAG)) {
                ArrayList<String> issuingAgentAddressList = new ArrayList<>();
                issuingAgentAddressList.add(orgRow.getAddressId());
                Map<Long, AddressDataV1> issuingAgentAddressDataMap = fetchAddressData(issuingAgentAddressList);
                String addressId = orgRow.getAddressId();
                var issuingAgentName = StringUtility.convertToString(orgRow.getOrgData().get(PartiesConstants.FULLNAME));
                awbShipmentInfo.setIssuingAgentName(issuingAgentName == null ? issuingAgentName : issuingAgentName.toUpperCase()); // extract from orgdata
                constructIssuingAgentAddress(awbShipmentInfo, orgRow.getAddressData(), alpha2DigitToCountry, issuingAgentAddressDataMap.get(Long.valueOf(addressId)));

                setAwbShipmentInfoCustomValue(shipmentDetails, awbCargoInfo, orgRow, issuingAgentAddressDataMap, awbShipmentInfo);
                // awbOtherInfoRow.setExecutedAt(getCityId(orgRow.OrgId)); // fetch from master data
                // awbCargoInfo.CustomOriginCode(getCountryCode(orgRow.OrgCountry)); // fetch from master data
            }
        }

        try {
            TenantModel tenantModel = jsonHelper.convertValue(tenantResponse.getEntity(), TenantModel.class);
            setTenantFieldsInAwbShipmentInfo(awbShipmentInfo, tenantModel);
            if (awbShipmentInfo.getIssuingAgentName() == null || awbShipmentInfo.getIssuingAgentName().isEmpty()) {
                populateIssuingAgent(awbShipmentInfo, tenantModel, awbCargoInfo);
            }
        } catch (Exception e) {
            throw new RunnerException(String.format("Error while populating tenant fields in AwbShipmentInfo %s", e.getMessage()));
        }

        return awbShipmentInfo;
    }

    private void setAwbShipmentInfoCustomValue(ShipmentDetails shipmentDetails, AwbCargoInfo awbCargoInfo, Parties orgRow, Map<Long, AddressDataV1> issuingAgentAddressIdToEntityMap, AwbShipmentInfo awbShipmentInfo) {
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
    }

    private Map<Long, AddressDataV1> getAddressDataV1Map(ArrayList<String> issuingAgentAddressList) {
        Map<Long, AddressDataV1> issuingAgentAddressIdToEntityMap = new HashMap<>();
        if (!CommonUtils.listIsNullOrEmpty(issuingAgentAddressList)) {
            CommonV1ListRequest issuingAgentAddressRequest = createCriteriaToFetchAddressList(issuingAgentAddressList);
            V1DataResponse issuingAgentAddressResponse = v1Service.addressList(issuingAgentAddressRequest);
            List<AddressDataV1> issuingAgentEntityAddressList = jsonHelper.convertValueToList(issuingAgentAddressResponse.entities, AddressDataV1.class);
            issuingAgentAddressIdToEntityMap = issuingAgentEntityAddressList.stream()
                    .collect(Collectors.toMap(AddressDataV1::getId, entity -> entity));
        }
        return issuingAgentAddressIdToEntityMap;
    }

    private void processShipmentConsignerConsginee(ShipmentDetails shipmentDetails, AwbShipmentInfo awbShipmentInfo, Map<String, String> alpha2DigitToCountry, Map<Long, AddressDataV1> addressDataV1Map) {
        AddressDataV1 shipperAddressData = null;
        if(shipmentDetails.getConsigner() != null && shipmentDetails.getConsigner().getAddressId() != null) {
            String consignerAddressId = shipmentDetails.getConsigner().getAddressId();
            shipperAddressData = addressDataV1Map.get(Long.valueOf(consignerAddressId));
        }
        var shipperName = StringUtility.convertToString(shipmentDetails.getConsigner() != null && shipmentDetails.getConsigner().getOrgData() != null ? shipmentDetails.getConsigner().getOrgData().get(PartiesConstants.FULLNAME) : "");
        awbShipmentInfo.setShipperName(shipperName == null ? shipperName : shipperName.toUpperCase());
        constructShipperAddress(awbShipmentInfo, shipmentDetails.getConsigner() != null ? shipmentDetails.getConsigner().getAddressData() : null, alpha2DigitToCountry, shipperAddressData);
        AddressDataV1 consigneeAddressData = null;
        if(shipmentDetails.getConsignee() != null && shipmentDetails.getConsignee().getAddressId() != null) {
            String consigneeAddressId = shipmentDetails.getConsignee().getAddressId();
            consigneeAddressData = addressDataV1Map.get(Long.valueOf(consigneeAddressId));
        }
        var consigneeName = StringUtility.convertToString(shipmentDetails.getConsignee() != null && shipmentDetails.getConsignee().getOrgData() != null ? shipmentDetails.getConsignee().getOrgData().get(PartiesConstants.FULLNAME) : "");
        awbShipmentInfo.setConsigneeName(consigneeName == null ? consigneeName : consigneeName.toUpperCase());
        var consigneeAddress = AwbUtility.constructAddressForAwb(shipmentDetails.getConsignee() != null ? shipmentDetails.getConsignee().getAddressData() : null);
        awbShipmentInfo.setConsigneeAddress(consigneeAddress == null ? consigneeAddress : consigneeAddress.toUpperCase());
        constructConsigneeAddress(awbShipmentInfo, shipmentDetails.getConsignee() != null ? shipmentDetails.getConsignee().getAddressData() : null, alpha2DigitToCountry, consigneeAddressData);
        awbShipmentInfo.setConsigneeReferenceNumber(shipmentDetails.getConsignee() != null ? shipmentDetails.getConsignee().getId().toString() : null);
    }

    private CommonV1ListRequest createCriteriaToFetchAddressList(ArrayList<String> addressIdList) {
        List<Object> addressIdField = new ArrayList<>(List.of(Constants.ID));
        List<Object> addressCriteria = new ArrayList<>(List.of(addressIdField, Constants.IN, List.of(addressIdList)));
        return CommonV1ListRequest.builder().criteriaRequests(addressCriteria).build();
    }

    private List<AwbNotifyPartyInfo> generateAwbNotifyPartyinfo(ShipmentDetails shipmentDetails, CreateAwbRequest request, Map<Long, AddressDataV1> addressDataV1Map) {
        AddressDataV1 notifyPartyAddressData = null;
        if(shipmentDetails.getAdditionalDetails() != null && shipmentDetails.getAdditionalDetails().getNotifyParty() != null && shipmentDetails.getAdditionalDetails().getNotifyParty().getAddressId() != null) {
            String notifyPartyAddressId = shipmentDetails.getAdditionalDetails().getNotifyParty().getAddressId();
            notifyPartyAddressData = addressDataV1Map.get(Long.valueOf(notifyPartyAddressId));
        }
        if (shipmentDetails.getAdditionalDetails() != null &&
                shipmentDetails.getAdditionalDetails().getNotifyParty() != null &&
                shipmentDetails.getAdditionalDetails().getNotifyParty().getId() != null) {
            Map<String, String> alpha2DigitToCountry = new HashMap<>();
            var shipmentNotifyParty = shipmentDetails.getAdditionalDetails().getNotifyParty();
            if (shipmentNotifyParty.getAddressData() != null && shipmentNotifyParty.getAddressData().containsKey(PartiesConstants.COUNTRY)) {
                List<String> alpha3CountriesList = new ArrayList<>();
                alpha3CountriesList = masterDataUtils.addAlpha3Country(shipmentNotifyParty.getAddressData(), alpha3CountriesList);
                alpha2DigitToCountry = masterDataUtils.getCountriesMasterListData(alpha3CountriesList);
            }
            AwbNotifyPartyInfo notifyPartyInfo = new AwbNotifyPartyInfo();
            notifyPartyInfo.setIsShipmentCreated(true);
            var name = StringUtility.convertToString(shipmentNotifyParty.getOrgData().get(PartiesConstants.FULLNAME));
            notifyPartyInfo.setName(name == null ? name : name.toUpperCase());
            notifyPartyInfo.setEntityId(shipmentDetails.getId());
            notifyPartyInfo.setEntityType(request.getAwbType());
            constructNotifyPartyAddress(notifyPartyInfo, shipmentNotifyParty.getAddressData(), alpha2DigitToCountry, notifyPartyAddressData);
            // notifyPartyInfo.setAddressId(shipmentNotifyParty.getAddressData()); // field missing: AddressId
            notifyPartyInfo.setNotifyOrgId(shipmentNotifyParty.getId());
            return Arrays.asList(notifyPartyInfo);
        }
        return null;
    }

    private List<AwbRoutingInfo> generateAwbRoutingInfo(ShipmentDetails shipmentDetails, CreateAwbRequest request) {
        if (shipmentDetails.getRoutingsList() != null && !shipmentDetails.getRoutingsList().isEmpty()) {
            var sortedRoutingList = shipmentDetails.getRoutingsList().stream().filter(route -> Objects.equals(route.getMode(), Constants.TRANSPORT_MODE_AIR) && Objects.equals(route.getCarriage(), RoutingCarriage.MAIN_CARRIAGE)).collect(Collectors.toList());
            if (sortedRoutingList != null && !sortedRoutingList.isEmpty()) {
                List<AwbRoutingInfo> res = new ArrayList<>();
                Collections.sort(sortedRoutingList, Comparator.comparing(Routings::getLeg));
                Long leg = 1L;
                for (var route : sortedRoutingList) {
                    AwbRoutingInfo awbRoutingInfo = new AwbRoutingInfo();
                    awbRoutingInfo.setIsShipmentCreated(true);
                    awbRoutingInfo.setOriginPortName(route.getPol());
                    awbRoutingInfo.setDestinationPortName(route.getPod());
                    awbRoutingInfo.setByCarrier(route.getCarrier());
                    awbRoutingInfo.setFlightNumber(Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getEnableRouteMaster()) ? route.getVoyage() : route.getFlightNumber());
                    awbRoutingInfo.setFlightDate(route.getEtd());
                    awbRoutingInfo.setEta(route.getEta());
                    awbRoutingInfo.setEntityId(shipmentDetails.getId());
                    awbRoutingInfo.setEntityType(request.getAwbType());
                    awbRoutingInfo.setLeg(leg++);
                    res.add(awbRoutingInfo);
                }
                return res;
            }
        } else if (shipmentDetails.getCarrierDetails() != null &&
                shipmentDetails.getCarrierDetails().getOriginPort() != null &&
                shipmentDetails.getCarrierDetails().getDestinationPort() != null
        ) {
            var flightDate = shipmentDetails.getCarrierDetails().getEtd();
            var eta = shipmentDetails.getCarrierDetails().getEta();
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
            routingInfo.setEta(eta);
            routingInfo.setEntityId(shipmentDetails.getId());
            routingInfo.setEntityType(request.getAwbType());
            return Arrays.asList(routingInfo);
        }
        return null;
    }

    private AwbCargoInfo generateAwbCargoInfo(ShipmentDetails shipmentDetails, CreateAwbRequest request, List<AwbPackingInfo> awbPackingList, AwbCargoInfo awbCargoInfo, List<AwbGoodsDescriptionInfo> awbGoodsDescriptionInfos, TenantModel tenantModel) {
        if (awbCargoInfo == null) {
            awbCargoInfo = new AwbCargoInfo();
        }
        GenerateAwbPaymentInfoRequest generateAwbPaymentInfoRequest = new GenerateAwbPaymentInfoRequest();
        generateAwbPaymentInfoRequest.setAwbCargoInfo(awbCargoInfo);
        generateAwbPaymentInfoRequest.setAwbPackingInfo(awbPackingList);
        generateAwbPaymentInfoRequest.setAwbGoodsDescriptionInfo(awbGoodsDescriptionInfos);
        generateAwbPaymentInfoRequest.setIsFromShipment(true);
        generateAwbPaymentInfoRequest.setPackUpdate(false);
        awbCargoInfo.setEntityId(shipmentDetails.getId());
        awbCargoInfo.setEntityType(request.getAwbType());
        awbCargoInfo.setCustomsValue(BigDecimal.valueOf(0.0));
        awbCargoInfo.setCurrency(UserContext.getUser().getCompanyCurrency());
        awbCargoInfo.setHandlingInfo(getHandlingInfo(MasterDataType.HAWB_GENERATION, awbPackingList, shipmentDetails.getContainsHazardous()));
        awbCargoInfo.setAccountingInfo(awbCargoInfo.getAccountingInfo() == null ? null : awbCargoInfo.getAccountingInfo().toUpperCase());
        awbCargoInfo.setOtherInfo(awbCargoInfo.getOtherInfo() == null ? null : awbCargoInfo.getOtherInfo().toUpperCase());
        awbCargoInfo.setShippingInformation(StringUtility.isEmpty(shipmentDetails.getOrderManagementNumber()) ? null : String.format(AwbConstants.ORDER_NUMBER, shipmentDetails.getOrderManagementNumber()));
        awbCargoInfo.setShippingInformationOther(awbCargoInfo.getShippingInformationOther() == null ? null : awbCargoInfo.getShippingInformationOther().toUpperCase());
        if(shipmentDetails.getIncoterms() != null && AwbConstants.incotermChargeCodeMap.containsKey(shipmentDetails.getIncoterms()))
            awbCargoInfo.setChargeCode(AwbConstants.incotermChargeCodeMap.get(shipmentDetails.getIncoterms()));

        // Set the RA Number and Country Code from branch default address
        populateCsdInfo(awbCargoInfo, tenantModel);
        // Set Screening Status, Other info (in case of AOM), security Status, screening status, exemption code
        AdditionalDetails additionalDetails = shipmentDetails.getAdditionalDetails();
        awbCargoInfo.setScreeningStatus(jsonHelper.convertValueToList(additionalDetails.getScreeningStatus(), String.class));
        awbCargoInfo.setOtherMethod(additionalDetails.getAomFreeText());
        awbCargoInfo.setSecurityStatus(shipmentDetails.getSecurityStatus());
        awbCargoInfo.setExemptionCode(additionalDetails.getExemptionCodes());

        return awbCargoInfo;
    }

    // Populate the RA Number and Country Code from branch default address
    public void populateCsdInfo(AwbCargoInfo awbCargoInfo, TenantModel tenantModel) {
        if (Objects.nonNull(tenantModel.getDefaultAddressId())) {
            CommonV1ListRequest addressRequest = new CommonV1ListRequest();
            List<Object> addressField = new ArrayList<>(List.of("Id"));
            List<Object> addressCriteria = new ArrayList<>(List.of(addressField, "=", tenantModel.getDefaultAddressId()));
            addressRequest.setCriteriaRequests(addressCriteria);
            V1DataResponse addressResponse = v1Service.addressList(addressRequest);
            List<EntityTransferAddress> addressList = jsonHelper.convertValueToList(addressResponse.entities, EntityTransferAddress.class);
            EntityTransferAddress address = addressList.stream().findFirst().orElse(EntityTransferAddress.builder().build());

            awbCargoInfo.setRaNumber(address.getKCRANumber());
            awbCargoInfo.setRaExpiryDate(StringUtility.isNotEmpty(address.getKCRAExpiry()) ? LocalDateTime.parse(address.getKCRAExpiry()) : null);
            if (StringUtility.isNotEmpty(address.getCountry()))
                awbCargoInfo.setCountryCode(address.getCountry().length() == 2 ? address.getCountry() : CountryListHelper.ISO3166.getAlpha2IfAlpha3(address.getCountry()));
        }
    }

    private String getCountryCode(String country) {
        if (StringUtility.isEmpty(country)) {
            return null;
        }
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
            if (responseList != null && !responseList.isEmpty())
                return responseList.get(0).getIdentifier1();
        } catch (Exception ignored) {
            log.info(Constants.IGNORED_ERROR_MSG);
        }
        return null;
    }

    private AwbOtherInfo generateAwbOtherInfo(ShipmentDetails shipmentDetails, CreateAwbRequest request) throws RunnerException {
        AwbOtherInfo awbOtherInfo = new AwbOtherInfo();
        awbOtherInfo.setEntityId(shipmentDetails.getId());
        awbOtherInfo.setEntityType(request.getAwbType());
        var shipperName = StringUtility.convertToString(shipmentDetails.getConsigner() != null ? shipmentDetails.getConsigner().getOrgData().get(PartiesConstants.FULLNAME) : "");
        awbOtherInfo.setShipper(shipperName == null ? null : shipperName.toUpperCase());
        awbOtherInfo.setExecutedOn(jsonHelper.convertValue(DateTimeFormatter.ofPattern(Constants.YYYY_MM_DD_T_HH_MM_SS).format(LocalDateTime.now()), LocalDateTime.class));
        awbOtherInfo.setExecutedAt(executedAt);
        getAwbOtherInfoMasterData(awbOtherInfo, request.getAwbType());
        try {
            awbOtherInfo.setBranch(getTenantBranch());
            awbOtherInfo.setLegalCompanyName(getLegalCompanyName());
            CompanyDto companyDetails = fetchCompanyDetails();
            if (companyDetails != null) {
                awbOtherInfo.setAddress1(companyDetails.getAddress1() != null ? companyDetails.getAddress1().toUpperCase() : "");
                awbOtherInfo.setAddress2(companyDetails.getAddress2() != null ? companyDetails.getAddress2().toUpperCase() : "");
                awbOtherInfo.setState(companyDetails.getState() != null ? companyDetails.getState() : "");
                awbOtherInfo.setCity(companyDetails.getCity() != null ? companyDetails.getCity() : "");
                awbOtherInfo.setPincode(companyDetails.getZipPostCode() != null ? companyDetails.getZipPostCode() : "");
                setCountryDetails(companyDetails, awbOtherInfo);
            }
        } catch (Exception e) {
            throw new RunnerException(String.format("Error while populating default awb other info %s", e.getMessage()));
        }
        return awbOtherInfo;
    }

    private void setCountryDetails(CompanyDto companyDetails, AwbOtherInfo awbOtherInfo) {
        String country = companyDetails.getCountry();
        List<String> alpha3CountriesList = new ArrayList<>();
        if (country != null) {
            if (country.length() == 2)
                country = CountryListHelper.ISO3166.getAlpha3FromAlpha2(country);
            if (country.length() == 3)
                alpha3CountriesList.add(country);
        }
        Map<String, String> alpha2DigitToCountry = masterDataUtils.getCountriesMasterListData(alpha3CountriesList);
        if (country != null && country.length() == 3)
            country = CountryListHelper.ISO3166.getAlpha2FromAlpha3(country);
        awbOtherInfo.setCountryCode(country != null ? country : "");
        awbOtherInfo.setCountryName(alpha2DigitToCountry != null ? alpha2DigitToCountry.get(country) : "");
    }

    private List<AwbGoodsDescriptionInfo> generateAwbGoodsDescriptionInfo(ShipmentDetails shipmentDetails, CreateAwbRequest request, List<AwbPackingInfo> awbPackingList) {
        AwbGoodsDescriptionInfo awbGoodsDescriptionInfo = new AwbGoodsDescriptionInfo();
        awbGoodsDescriptionInfo.setEntityId(shipmentDetails.getId());
        awbGoodsDescriptionInfo.setEntityType(request.getAwbType());
        awbGoodsDescriptionInfo.setIsShipmentCreated(true);
        awbGoodsDescriptionInfo.setGrossWt(shipmentDetails.getWeight());
        awbGoodsDescriptionInfo.setGrossWtUnit(shipmentDetails.getWeightUnit());
        awbGoodsDescriptionInfo.setGrossVolume(shipmentDetails.getVolume() != null ? shipmentDetails.getVolume().setScale(3, RoundingMode.HALF_UP) : BigDecimal.ZERO.setScale(3, RoundingMode.HALF_UP));
        awbGoodsDescriptionInfo.setGrossVolumeUnit(shipmentDetails.getVolumeUnit());
        awbGoodsDescriptionInfo.setPiecesNo(shipmentDetails.getNoOfPacks());
        awbGoodsDescriptionInfo.setChargeableWt(shipmentDetails.getChargable() != null ?
                AwbUtility.roundOffAirShipment(shipmentDetails.getChargable().doubleValue()) : null);
        awbGoodsDescriptionInfo.setGuid(UUID.randomUUID());
        awbGoodsDescriptionInfo.setNtrQtyGoods(shipmentDetails.getGoodsDescription() != null ? StringUtility.toUpperCase(shipmentDetails.getGoodsDescription()) : "");
        int noOfPacks = 0;
        if (awbPackingList != null) {
            Set<String> uniqueHsCodes = new LinkedHashSet<>();
            for (var awbPacking : awbPackingList) {
                awbPacking.setAwbGoodsDescriptionInfoGuid(awbGoodsDescriptionInfo.getGuid());
                noOfPacks += Integer.parseInt(Objects.isNull(awbPacking.getPacks()) ? "0" : awbPacking.getPacks());

                // Handle unique hscode
                String hscode = awbPacking.getHsCode();
                if (hscode != null && !hscode.isEmpty()) {
                    uniqueHsCodes.add(hscode);
                }
            }
            awbGoodsDescriptionInfo.setPiecesNo(noOfPacks);
            awbGoodsDescriptionInfo.setHsCode(String.join(",", uniqueHsCodes));
        }
        awbGoodsDescriptionInfo.setAwbPackingInfo(awbPackingList);
        return Arrays.asList(awbGoodsDescriptionInfo);
    }

    private List<AwbPackingInfo> generateAwbPackingInfo(ShipmentDetails shipmentDetails, List<Packing> packings) {
        Map<Long, String> map = new HashMap<>();
        if (shipmentDetails.getContainersList() != null && !shipmentDetails.getContainersList().isEmpty())
            map = shipmentDetails.getContainersList().stream().collect(Collectors.toMap(Containers::getId, Containers::getContainerNumber));
        if (packings != null && !packings.isEmpty()) {
            List<AwbPackingInfo> awbPackingList = new ArrayList<>();
            for (var packing : packings) {
                AwbPackingInfo awbPacking = new AwbPackingInfo();
                awbPacking.setGuid(packing.getGuid());
                awbPacking.setDgGoodsId(packing.getDGGoodsId());
                awbPacking.setDgSubstanceId(packing.getDGSubstanceId());
                awbPacking.setPacks(packing.getPacks());
                awbPacking.setPacksType(packing.getPacksType());
                if (packing.getContainerId() != null && map.containsKey(packing.getContainerId()))
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
                awbPackingList.add(awbPacking);
            }

            return awbPackingList;
        }
        return null;
    }

    public ResponseEntity<IRunnerResponse> createV1Awb(CommonRequestModel commonRequestModel, boolean checkForSync) {
        return ResponseHelper.buildSuccessResponse();
    }

    private void updateAwbOtherChargesInfo(List<AwbOtherChargesInfo> otherChargesInfos) {
        if (otherChargesInfos != null && !otherChargesInfos.isEmpty()) {
            otherChargesInfos.stream().map(i -> {
                if (i.getGuid() == null)
                    i.setGuid(UUID.randomUUID());
                return i;
            }).toList(); //NOSONAR
        }
    }

    List<Awb> getLinkedAwbFromMawb(Long mawbId) {
        List<MawbHawbLink> mawbHawbLinks = mawbHawbLinkDao.findByMawbId(mawbId);

        // Fetch all the awb records with the mapped hawbId
        return awbDao.findByIds(mawbHawbLinks.stream().map(MawbHawbLink::getHawbId).toList());
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

    @SuppressWarnings("java:S3655")
    private ResponseEntity<IRunnerResponse> resetAwb(CommonRequestModel commonRequestModel) throws RunnerException {
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

        Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(awb.getShipmentId());
        Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findById(awb.getConsolidationId());

        ArrayList<String> shipmentAddressIds = new ArrayList<>();
        if(shipmentDetails.isPresent()) {
            shipmentAddressIds = collectAddressIds(shipmentDetails.get());
        }
        Map<Long, AddressDataV1> shipmentAddressIdToAddressEntityMap = fetchAddressData(shipmentAddressIds);

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
                if (resetAwbRequest.getAwbType().equals(Constants.MAWB)) {
                    processMAWBAllCase(consolidationDetails.get(), createAwbRequest, awb, printType);
                } else {
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
                CarrierDetails carrierDetails;
                AwbShipmentInfo awbShipmentInfo = awb.getAwbShipmentInfo();
                if (resetAwbRequest.getAwbType().equals(Constants.MAWB)) {
                    awb.setAwbRoutingInfo(generateMawbRoutingInfo(consolidationDetails.get(), createAwbRequest));
                    carrierDetails = consolidationDetails.get().getCarrierDetails();
                }
                else
                {
                    awb.setAwbRoutingInfo(generateAwbRoutingInfo(shipmentDetails.get(), createAwbRequest));
                    carrierDetails = shipmentDetails.get().getCarrierDetails();
                }
                awbShipmentInfo.setFirstCarrier(carrierDetails.getShippingLine());
                setAwbShipmentInfoUnLocationData(awbShipmentInfo, carrierDetails);

                break;
            }
            case AWB_NOTIFY_PARTY_INFO: {
                if (resetAwbRequest.getAwbType().equals(Constants.MAWB))
                    awb.setAwbNotifyPartyInfo(generateMawbNotifyPartyinfo(consolidationDetails.get(), createAwbRequest));
                else
                    awb.setAwbNotifyPartyInfo(generateAwbNotifyPartyinfo(shipmentDetails.get(), createAwbRequest, shipmentAddressIdToAddressEntityMap));
                break;
            }
            case AWB_PACKS_AND_GOODS: {
                processAwbPacksAndGoods(resetAwbRequest, consolidationDetails, awb, createAwbRequest, awbId, shipmentDetails);
                break;
            }
            case AWB_OTHER_CHARGES_INFO: {
                awb.setAwbOtherChargesInfo(null);
                awb.setAwbPaymentInfo(null);
                break;
            }
            case AWB_OCI_INFO: {
                awb.setAwbOciInfo(null);
                awb.setOciInfo(OCIInfo.builder().otherIdentityInfo(OtherIdentityInfo.builder().iaIpAddress(convertIpFormat(getClientIp())).irIpAddress(convertIpFormat(getClientIp())).build()).build());
                break;
            }
        }
        awb.setId(resetAwbRequest.getId());
        awb.setAirMessageResubmitted(false);
        awb.setOriginalPrintedAt(originalPrintedAt);
        awb = awbDao.save(awb);
        syncAwb(awb, SaveStatus.RESET);

        return ResponseHelper.buildSuccessResponse(convertEntityToDto(awb));
    }

    @SuppressWarnings("java:S3655")
    private void processAwbPacksAndGoods(ResetAwbRequest resetAwbRequest, Optional<ConsolidationDetails> consolidationDetails, Awb awb, CreateAwbRequest createAwbRequest, Long awbId, Optional<ShipmentDetails> shipmentDetails) throws RunnerException {
        if (resetAwbRequest.getAwbType().equals(Constants.MAWB)) {
            PackSummaryResponse packSummary = packingService.calculatePackSummary(consolidationDetails.get().getPackingList(), consolidationDetails.get().getTransportMode(), consolidationDetails.get().getContainerCategory(), new ShipmentMeasurementDetailsDto());
            awb.setAwbGoodsDescriptionInfo(generateMawbGoodsDescriptionInfo(consolidationDetails.get(), createAwbRequest, null, packSummary));
            updateLinkHawbMawb(consolidationDetails.get(), awbId);
            getMawnLinkPacks(awb, true, null);
        } else {
            awb.setAwbPackingInfo(generateAwbPackingInfo(shipmentDetails.get(), shipmentDetails.get().getPackingList()));
            awb.setAwbGoodsDescriptionInfo(generateAwbGoodsDescriptionInfo(shipmentDetails.get(), createAwbRequest, awb.getAwbPackingInfo()));
        }
    }

    private void processMAWBAllCase(ConsolidationDetails consolidationDetails, CreateAwbRequest createAwbRequest, Awb awb, PrintType printType) throws RunnerException {
        List<AwbPackingInfo> mawbPackingInfo = new ArrayList<>();
        List<Long> shipmentDetailsIdList = consolidationDetails.getShipmentsList()
                .stream()
                .map(ShipmentDetails::getId)
                .toList();
        List<Awb> awbList = awbDao.findByShipmentIdList(shipmentDetailsIdList);
        Map<Long, Awb> shipmentIdToAwbMap = awbList.stream()
                .collect(Collectors.toMap(Awb::getShipmentId, awb1 -> awb1));
        if (awbList.size() != shipmentDetailsIdList.size()) {
            throw new ValidationException(AwbConstants.GENERATE_HAWB_BEFORE_MAWB_EXCEPTION);
        }
        for (Long shipmentId : shipmentDetailsIdList) {
            Awb linkAwb = shipmentIdToAwbMap.get(shipmentId);
            if (linkAwb.getAwbPackingInfo() != null) {
                mawbPackingInfo.addAll(linkAwb.getAwbPackingInfo());
            }
        }
        Awb resetAwb = generateMawb(createAwbRequest, consolidationDetails, mawbPackingInfo);
        awb.setAwbShipmentInfo(resetAwb.getAwbShipmentInfo());
        awb.setAwbNotifyPartyInfo(resetAwb.getAwbNotifyPartyInfo());
        awb.setAwbRoutingInfo(resetAwb.getAwbRoutingInfo());
        awb.setAwbGoodsDescriptionInfo(resetAwb.getAwbGoodsDescriptionInfo());
        awb.setAwbCargoInfo(resetAwb.getAwbCargoInfo());
        awb.setAwbOtherInfo(resetAwb.getAwbOtherInfo());
        awb.setAwbOciInfo(resetAwb.getAwbOciInfo());
        awb.setOciInfo(resetAwb.getOciInfo());
        awb.setAwbOtherChargesInfo(resetAwb.getAwbOtherChargesInfo());
        awb.setAwbPaymentInfo(resetAwb.getAwbPaymentInfo());
        awb.setAwbSpecialHandlingCodesMappings(resetAwb.getAwbSpecialHandlingCodesMappings());
        // Link
        linkHawbMawb(awb, awbList, consolidationDetails.getInterBranchConsole());
        getMawnLinkPacks(awb, true, awbList);
        awb.setPrintType(printType);
        if (!awbList.isEmpty())
            updateSciFieldFromMawb(awb, awbList);
    }

    @Override
    @Transactional
    public ResponseEntity<IRunnerResponse> reset(CommonRequestModel commonRequestModel) throws RunnerException {
        return resetAwb(commonRequestModel);
    }

    @Override
    @Transactional
    public ResponseEntity<IRunnerResponse> partialAutoUpdateAwb(CommonRequestModel commonRequestModel) throws RunnerException {
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
        if (awbs.isEmpty()) {
            log.error("No Awb exist for given shipment to update");
            throw new ValidationException("No Awb exist for given shipment to update");
        }

        Awb awb = awbs.get(0);

        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();

        if (shipmentSettingsDetails.getRestrictAWBEdit() != null && shipmentSettingsDetails.getRestrictAWBEdit()) {
            ResetAwbRequest resetAwbRequest = ResetAwbRequest.builder()
                    .id(awb.getId())
                    .shipmentId(request.getShipmentId())
                    .consolidationId(request.getConsolidationId())
                    .awbType(request.getAwbType())
                    .resetType(AwbReset.ALL)
                    .build();
            return resetAwb(CommonRequestModel.buildRequest(resetAwbRequest));
        } else if (shipmentSettingsDetails.getAutoUpdateShipmentAWB() != null && shipmentSettingsDetails.getAutoUpdateShipmentAWB()) {
            updateAwbFromShipment(awb, request, shipmentSettingsDetails);
            awb = awbDao.save(awb);
        }
        log.info("AWB created successfully for Id {} with Request Id {}", awb.getId(), LoggerHelper.getRequestIdFromMDC());
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(awb));
    }

    private void updateAwbFromShipment(Awb awb, CreateAwbRequest request, ShipmentSettingsDetails shipmentSettingsDetails) {
        Optional<ShipmentDetails> optionalShipmentDetails = shipmentDao.findById(request.getShipmentId());
        ArrayList<String> addressIds = collectAddressIds(optionalShipmentDetails.get());
        Map<Long, AddressDataV1> addressDataV1Map = fetchAddressData(addressIds);
        if (optionalShipmentDetails.isEmpty()) {
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        ShipmentDetails shipmentDetails = optionalShipmentDetails.get();

        // fetch all packings
        List<Packing> packings = shipmentDetails.getPackingList();


        HawbLockSettings hawbLockSettings = shipmentSettingsDetails.getHawbLockSettings() != null ? shipmentSettingsDetails.getHawbLockSettings() : new HawbLockSettings();
        MawbLockSettings mawbLockSettings = shipmentSettingsDetails.getMawbLockSettings() != null ? shipmentSettingsDetails.getMawbLockSettings() : new MawbLockSettings();
        Map<String, Boolean> awbGroupLocks = AwbUtility.getGroupLockStatus(awb, hawbLockSettings, mawbLockSettings);
        Integer totalPacksCount = 0;
        updateShipmentPackingInfoToAwb(shipmentDetails, packings, awb, request, hawbLockSettings, mawbLockSettings, totalPacksCount, awbGroupLocks);
        updateShipmentInfoToAwb(shipmentDetails, request, awb, hawbLockSettings, mawbLockSettings, addressDataV1Map, awbGroupLocks);
        updateShipmentNotifyPartyinfoToAwb(shipmentDetails, request, awb, hawbLockSettings, mawbLockSettings, addressDataV1Map, awbGroupLocks);
        updateShipmemtRoutingInfoToAwb(shipmentDetails, request, awb, hawbLockSettings, mawbLockSettings, awbGroupLocks);
        updateAwbGoodsDescriptionInfoFromShipment(shipmentDetails, request, awb, hawbLockSettings, mawbLockSettings, awbGroupLocks);
        updateAwbCargoInfoFromShipment(shipmentDetails, request, awb, hawbLockSettings, mawbLockSettings, awbGroupLocks);
        updateAwbOtherInfoFromShipment(shipmentDetails, request, awb, hawbLockSettings, mawbLockSettings, awbGroupLocks);
    }

    private void updateShipmentPackingInfoToAwb(ShipmentDetails shipmentDetails, List<Packing> packings, Awb awb, CreateAwbRequest request, HawbLockSettings hawbLockSettings, MawbLockSettings mawbLockSettings, Integer totalPacksCount, Map<String, Boolean> awbGroupLocks) {
        if(!Boolean.TRUE.equals(awbGroupLocks.get(PACKING_LOCK_GROUP))) {
            return;
        }
        Map<UUID, Packing> packMap = new HashMap<>();
        packings.forEach(pack -> packMap.put(pack.getGuid(), pack));
        totalPacksCount = getTotalPacksCount(awb, request, hawbLockSettings, mawbLockSettings, totalPacksCount, packMap);

        Map<Long, String> map = new HashMap<>();
        if (shipmentDetails.getContainersList() != null && !shipmentDetails.getContainersList().isEmpty())
            map = shipmentDetails.getContainersList().stream().collect(Collectors.toMap(Containers::getId, Containers::getContainerNumber));

        if (!packMap.isEmpty()) {
            for (var packing : packMap.values()) {
                AwbPackingInfo awbPacking = new AwbPackingInfo();
                awbPacking.setGuid(packing.getGuid());
                awbPacking.setDgGoodsId(packing.getDGGoodsId());
                awbPacking.setDgSubstanceId(packing.getDGSubstanceId());
                awbPacking.setPacks(packing.getPacks());
                awbPacking.setPacksType(packing.getPacksType());
                if (packing.getContainerId() != null && map.containsKey(packing.getContainerId()))
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
                if (awb.getAwbPackingInfo() == null) {
                    awb.setAwbPackingInfo(new ArrayList<>());
                }
                awb.getAwbPackingInfo().add(awbPacking);
            }
        }
    }

    private Integer getTotalPacksCount(Awb awb, CreateAwbRequest request, HawbLockSettings hawbLockSettings, MawbLockSettings mawbLockSettings, Integer totalPacksCount, Map<UUID, Packing> packMap) {
        List<AwbPackingInfo> deletedList = new ArrayList<>();
        if (awb.getAwbPackingInfo() != null && !awb.getAwbPackingInfo().isEmpty()) {
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
        return totalPacksCount;
    }

    private void updateShipmentPackingFieldToHbl(Packing packing, AwbPackingInfo awbPackingInfo, CreateAwbRequest request, HawbLockSettings hawbLockSettings, MawbLockSettings mawbLockSettings) {
        awbPackingInfo.setPacks(packing.getPacks());
        awbPackingInfo.setPacksType(packing.getPacksType());
        awbPackingInfo.setOrigin(packing.getOrigin());
        awbPackingInfo.setPackingOrder(packing.getPackingOrder());
        updateShipmentPackingAwbFields(packing, awbPackingInfo, request, hawbLockSettings, mawbLockSettings);
        awbPackingInfo.setMarksnNums(packing.getMarksnNums());
        awbPackingInfo.setCountryCode(packing.getCountryCode());
        awbPackingInfo.setGoodsDescription(packing.getGoodsDescription() == null ? null : packing.getGoodsDescription().toUpperCase());
        awbPackingInfo.setReferenceNumber(packing.getReferenceNumber());
        awbPackingInfo.setInspections(packing.getInspections());
        awbPackingInfo.setDgClass(packing.getDGClass());
        awbPackingInfo.setDgSubstanceId(packing.getDGSubstanceId());
        updateTempAwbShipmentFields(packing, awbPackingInfo, request, hawbLockSettings, mawbLockSettings);
        awbPackingInfo.setCommodity(packing.getCommodity());
        awbPackingInfo.setHsCode(packing.getHSCode());
        awbPackingInfo.setUnNumberAir(packing.getUnNumberAir());
        awbPackingInfo.setDgClassAir(packing.getDgClassAir());
        awbPackingInfo.setDgClassAirDescription(packing.getDgClassAirDescription());
    }

    private void updateTempAwbShipmentFields(Packing packing, AwbPackingInfo awbPackingInfo, CreateAwbRequest request, HawbLockSettings hawbLockSettings, MawbLockSettings mawbLockSettings) {
        awbPackingInfo.setMinTemp(packing.getMinTemp());
        awbPackingInfo.setMinTempUnit(packing.getMinTempUnit());
        awbPackingInfo.setMaxTemp(packing.getMaxTemp());
        awbPackingInfo.setMaxTempUnit(packing.getMaxTempUnit());
    }

    private void updateShipmentPackingAwbFields(Packing packing, AwbPackingInfo awbPackingInfo, CreateAwbRequest request, HawbLockSettings hawbLockSettings, MawbLockSettings mawbLockSettings) {
        awbPackingInfo.setLength(packing.getLength());
        awbPackingInfo.setLengthUnit(packing.getLengthUnit());
        awbPackingInfo.setWidth(packing.getWidth());
        awbPackingInfo.setWidthUnit(packing.getWidthUnit());
        awbPackingInfo.setHeight(packing.getHeight());
        awbPackingInfo.setHeightUnit(packing.getHeightUnit());
        awbPackingInfo.setWeight(packing.getWeight());
        awbPackingInfo.setWeightUnit(packing.getWeightUnit());
        awbPackingInfo.setVolume(packing.getVolume());
        awbPackingInfo.setVolumeUnit(packing.getVolumeUnit());
        awbPackingInfo.setNetWeight(packing.getNetWeight());
        awbPackingInfo.setNetWeightUnit(packing.getNetWeightUnit());
        awbPackingInfo.setVolumeWeight(packing.getVolumeWeight());
        awbPackingInfo.setVolumeWeightUnit(packing.getVolumeWeightUnit());
    }

    private void updateShipmentInfoToAwb(ShipmentDetails shipmentDetails, CreateAwbRequest request, Awb awb, HawbLockSettings hawbLockSettings, MawbLockSettings mawbLockSettings, Map<Long, AddressDataV1> addressDataV1Map, Map<String, Boolean> awbGroupLocks) {
        AwbShipmentInfo awbShipmentInfo = awb.getAwbShipmentInfo();
        Map<String, String> alpha2DigitToCountry = masterDataUtils.shipmentAddressCountryMasterData(shipmentDetails);

        if (Boolean.TRUE.equals(awbGroupLocks.get(AWB_NUMBER_LOCK_GROUP)))
            awbShipmentInfo.setAwbNumber(shipmentDetails.getHouseBill());

        if (Boolean.TRUE.equals(awbGroupLocks.get(SHIPPER_LOCK_GROUP))) {
            var shipperName = StringUtility.convertToString(shipmentDetails.getConsigner() != null ? shipmentDetails.getConsigner().getOrgData().get(PartiesConstants.FULLNAME) : "");
            awbShipmentInfo.setShipperName(shipperName == null ? shipperName : shipperName.toUpperCase());
            if(shipmentDetails.getConsigner()!=null && shipmentDetails.getConsigner().getAddressId() != null) {
                String consignerAddressId = shipmentDetails.getConsigner().getAddressId();
                constructShipperAddress(awbShipmentInfo, shipmentDetails.getConsigner() != null ? shipmentDetails.getConsigner().getAddressData() : null, alpha2DigitToCountry, addressDataV1Map.get(Long.valueOf(consignerAddressId)));
            }
        }
        if(shipmentDetails.getConsignee() != null && shipmentDetails.getConsignee().getAddressId() != null && Boolean.TRUE.equals(awbGroupLocks.get(CONSIGNEE_LOCK_GROUP))) {
            String consigneeAddressId = shipmentDetails.getConsignee().getAddressId();
            setConsgineeNameAndAddress(shipmentDetails, request, hawbLockSettings, mawbLockSettings, awbShipmentInfo, alpha2DigitToCountry, addressDataV1Map.get(Long.valueOf(consigneeAddressId)));
        }
        setUnLocationData(shipmentDetails, request, hawbLockSettings, mawbLockSettings, awbShipmentInfo, awbGroupLocks);

        if (Boolean.TRUE.equals(awbGroupLocks.get(ROUTING_DETAILS_LOCK_GROUP)))
            awbShipmentInfo.setFirstCarrier(shipmentDetails.getCarrierDetails().getShippingLine());

        if (Boolean.TRUE.equals(awbGroupLocks.get(ISSUING_AGENT_LOCK_GROUP)))
            awbShipmentInfo.setIataCode(iataCode);

        processShipmentAddresses(shipmentDetails, request, hawbLockSettings, mawbLockSettings, awbShipmentInfo, alpha2DigitToCountry, addressDataV1Map, awbGroupLocks);
    }

    private void setConsgineeNameAndAddress(ShipmentDetails shipmentDetails, CreateAwbRequest request, HawbLockSettings hawbLockSettings, MawbLockSettings mawbLockSettings, AwbShipmentInfo awbShipmentInfo, Map<String, String> alpha2DigitToCountry, AddressDataV1 addressData) {
        var consigneeName = StringUtility.convertToString(shipmentDetails.getConsignee() != null ? shipmentDetails.getConsignee().getOrgData().get(PartiesConstants.FULLNAME) : "");
        awbShipmentInfo.setConsigneeName(consigneeName == null ? consigneeName : consigneeName.toUpperCase());
        constructConsigneeAddress(awbShipmentInfo, shipmentDetails.getConsignee() != null ? shipmentDetails.getConsignee().getAddressData() : null, alpha2DigitToCountry, addressData);
    }

    private void setUnLocationData(ShipmentDetails shipmentDetails, CreateAwbRequest request, HawbLockSettings hawbLockSettings, MawbLockSettings mawbLockSettings, AwbShipmentInfo awbShipmentInfo, Map<String, Boolean> awbGroupLocks) {
        if (Boolean.TRUE.equals(awbGroupLocks.get(ROUTING_DETAILS_LOCK_GROUP))) {
            setAwbShipmentInfoUnLocationData(awbShipmentInfo, shipmentDetails.getCarrierDetails());
        }
    }

    private void processShipmentAddresses(ShipmentDetails shipmentDetails, CreateAwbRequest request, HawbLockSettings hawbLockSettings, MawbLockSettings mawbLockSettings, AwbShipmentInfo awbShipmentInfo, Map<String, String> alpha2DigitToCountry, Map<Long, AddressDataV1> addressDataV1Map, Map<String, Boolean> awbGroupLocks) {
        for (var orgRow : shipmentDetails.getShipmentAddresses()) {
            if (orgRow.getType().equals(Constants.FAG)) {
                if(orgRow.getAddressId() != null && Boolean.TRUE.equals(awbGroupLocks.get(ISSUING_AGENT_LOCK_GROUP))) {
                    String addressId = orgRow.getAddressId();
                    setIssuingAgentAndAgentAddress(request, hawbLockSettings, mawbLockSettings, awbShipmentInfo, alpha2DigitToCountry, orgRow, addressDataV1Map.get(Long.valueOf(addressId)));
                    awbShipmentInfo.setIataCode(StringUtility.isEmpty(awbShipmentInfo.getIataCode())
                            ? StringUtility.convertToString(shipmentDetails.getConsignee().getOrgData().get(PartiesConstants.AGENT_IATA_CODE))
                            : awbShipmentInfo.getIataCode());
                    awbShipmentInfo.setAgentCASSCode(StringUtility.isEmpty(awbShipmentInfo.getAgentCASSCode())
                            ? StringUtility.convertToString(shipmentDetails.getConsignee().getOrgData().get(PartiesConstants.AGENT_CASS_CODE))
                            : awbShipmentInfo.getAgentCASSCode());
                }
            }
        }
    }

    private void setIssuingAgentAndAgentAddress(CreateAwbRequest request, HawbLockSettings hawbLockSettings, MawbLockSettings mawbLockSettings, AwbShipmentInfo awbShipmentInfo, Map<String, String> alpha2DigitToCountry, Parties orgRow, AddressDataV1 addressData) {
        var issuingAgentName = StringUtility.convertToString(orgRow.getOrgData().get(PartiesConstants.FULLNAME));
        awbShipmentInfo.setIssuingAgentName(issuingAgentName == null ? issuingAgentName : issuingAgentName.toUpperCase()); // extract from orgdata
        constructIssuingAgentAddress(awbShipmentInfo, orgRow.getAddressData(), alpha2DigitToCountry, addressData);
    }

    private void updateShipmentNotifyPartyinfoToAwb(ShipmentDetails shipmentDetails, CreateAwbRequest request, Awb awb, HawbLockSettings hawbLockSettings, MawbLockSettings mawbLockSettings, Map<Long, AddressDataV1> addressDataV1Map, Map<String, Boolean> awbGroupLocks) {
        if (!Boolean.TRUE.equals(awbGroupLocks.get(NOTIFY_LOCK_GROUP))) {
            return;
        }
        var party = shipmentDetails.getAdditionalDetails().getNotifyParty();
        boolean createNotifyParty = true;
        AwbNotifyPartyInfo deleteParty = new AwbNotifyPartyInfo();
        Map<String, String> alpha2DigitToCountry = masterDataUtils.shipmentAddressCountryMasterData(shipmentDetails);
        if (isPartyAddressContainsCountry(party)) {
            List<String> alpha3CountriesList = new ArrayList<>();
            alpha3CountriesList = masterDataUtils.addAlpha3Country(party.getAddressData(), alpha3CountriesList);
            alpha2DigitToCountry = masterDataUtils.getCountriesMasterListData(alpha3CountriesList);
        }
        if (awb.getAwbNotifyPartyInfo() != null && !awb.getAwbNotifyPartyInfo().isEmpty()) {
            for (var awbParty : awb.getAwbNotifyPartyInfo()) {
                if (awbParty.getIsShipmentCreated() != null && awbParty.getIsShipmentCreated()) {
                    createNotifyParty = false;
                    deleteParty = getDeleteParty(request, hawbLockSettings, mawbLockSettings, awbParty, party, alpha2DigitToCountry, deleteParty, addressDataV1Map);
                }
            }
            awb.getAwbNotifyPartyInfo().remove(deleteParty);
        }

        AwbNotifyPartyInfo awbParty = AwbNotifyPartyInfo.builder().build();
        if (party != null && createNotifyParty) {
            awbParty.setIsShipmentCreated(true);
            var name = StringUtility.convertToString(party.getOrgData().get(PartiesConstants.FULLNAME));
            awbParty.setName(name == null ? name : name.toUpperCase());
            awbParty.setAddress(AwbUtility.constructAddressForAwb(party.getAddressData()).toUpperCase());
            awbParty.setEntityId(shipmentDetails.getId());
            awbParty.setEntityType(request.getAwbType());
            if(party.getAddressId() != null) {
                String partyAddressId = party.getAddressId();
                constructNotifyPartyAddress(awbParty, party.getAddressData(), alpha2DigitToCountry, addressDataV1Map.get(Long.valueOf(partyAddressId)));
            }
            awbParty.setNotifyOrgId(party.getId());
            if (awb.getAwbNotifyPartyInfo() == null) {
                awb.setAwbNotifyPartyInfo(new ArrayList<>());
            }
            awb.getAwbNotifyPartyInfo().add(awbParty);
        }
    }

    private boolean isPartyAddressContainsCountry(Parties party) {
        return party != null && party.getAddressData() != null && party.getAddressData().containsKey(PartiesConstants.COUNTRY);
    }

    private AwbNotifyPartyInfo getDeleteParty(CreateAwbRequest request, HawbLockSettings hawbLockSettings, MawbLockSettings mawbLockSettings, AwbNotifyPartyInfo awbParty, Parties party, Map<String, String> alpha2DigitToCountry, AwbNotifyPartyInfo deleteParty, Map<Long, AddressDataV1> addressDataV1Map) {
        if (party != null) {
            var name = StringUtility.convertToString(party.getOrgData().get(PartiesConstants.FULLNAME));
            awbParty.setName(name == null ? name : name.toUpperCase());
            awbParty.setNotifyOrgId(party.getId());
            awbParty.setAddress(AwbUtility.constructAddressForAwb(party.getAddressData()).toUpperCase());
            if(party.getAddressId() != null) {
                String partyAddressId = party.getAddressId();
                constructNotifyPartyAddress(awbParty, party.getAddressData(), alpha2DigitToCountry, addressDataV1Map.get(Long.valueOf(partyAddressId)));
            }
        }
        else {
            deleteParty = awbParty;
        }
        return deleteParty;
    }

    private void updateShipmemtRoutingInfoToAwb(ShipmentDetails shipmentDetails, CreateAwbRequest request, Awb awb, HawbLockSettings hawbLockSettings, MawbLockSettings mawbLockSettings, Map<String, Boolean> awbGroupLocks) {
        if (!Boolean.TRUE.equals(awbGroupLocks.get(NEW_ROUTING_INFO_LOCK_GROUP))) {
            return;
        }
        boolean createRouting = true;
        AwbRoutingInfo deleteParty = new AwbRoutingInfo();
        if (awb.getAwbRoutingInfo() != null && !awb.getAwbRoutingInfo().isEmpty()) {
            for (var awbRoute : awb.getAwbRoutingInfo()) {
                if (isShipmentCreatedTrue(awbRoute)) {
                    createRouting = false;
                    if (shipmentDetails.getCarrierDetails() != null &&
                            shipmentDetails.getCarrierDetails().getOriginPort() != null &&
                            shipmentDetails.getCarrierDetails().getDestinationPort() != null) {
                        processShipmentFieldsLockedOrNot(shipmentDetails, request, hawbLockSettings, mawbLockSettings, awbRoute);
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
            addRoutingInfo(shipmentDetails, request, awb);
        }
    }

    private boolean isShipmentCreatedTrue(AwbRoutingInfo awbRoute) {
        return awbRoute.getIsShipmentCreated() != null && awbRoute.getIsShipmentCreated();
    }

    private void addRoutingInfo(ShipmentDetails shipmentDetails, CreateAwbRequest request, Awb awb) {
        var flightDate = Objects.equals(request.getAwbType(), Constants.DMAWB) ? shipmentDetails.getCarrierDetails().getEtd() : null;
        var eta = request.getAwbType().equalsIgnoreCase(Constants.DMAWB) ? shipmentDetails.getCarrierDetails().getEta() : null;
        AwbRoutingInfo routingInfo = new AwbRoutingInfo();
        routingInfo.setIsShipmentCreated(true);
        routingInfo.setOriginPortName(shipmentDetails.getCarrierDetails().getOriginPort());
        routingInfo.setDestinationPortName(shipmentDetails.getCarrierDetails().getDestinationPort());
        routingInfo.setByCarrier(shipmentDetails.getCarrierDetails().getShippingLine());
        routingInfo.setFlightNumber(shipmentDetails.getCarrierDetails().getFlightNumber());
        routingInfo.setFlightDate(flightDate);
        routingInfo.setEta(eta);
        routingInfo.setEntityId(shipmentDetails.getId());
        routingInfo.setEntityType(request.getAwbType());
        if (awb.getAwbRoutingInfo() == null) {
            awb.setAwbRoutingInfo(new ArrayList<>());
        }
        awb.getAwbRoutingInfo().add(routingInfo);
    }

    private void processShipmentFieldsLockedOrNot(ShipmentDetails shipmentDetails, CreateAwbRequest request, HawbLockSettings hawbLockSettings, MawbLockSettings mawbLockSettings, AwbRoutingInfo awbRoute) {
        awbRoute.setOriginPortName(shipmentDetails.getCarrierDetails().getOriginPort());
        awbRoute.setDestinationPortName(shipmentDetails.getCarrierDetails().getDestinationPort());
        awbRoute.setByCarrier(shipmentDetails.getCarrierDetails().getShippingLine());

        awbRoute.setFlightNumber(shipmentDetails.getCarrierDetails().getFlightNumber());
        var flightDate = Objects.equals(request.getAwbType(), Constants.DMAWB) ? shipmentDetails.getCarrierDetails().getEtd() : null;
        var eta = request.getAwbType().equalsIgnoreCase(Constants.DMAWB) ? shipmentDetails.getCarrierDetails().getEta() : null;
        awbRoute.setFlightDate(flightDate);
        awbRoute.setEta(eta);
    }

    private void updateAwbGoodsDescriptionInfoFromShipment(ShipmentDetails shipmentDetails, CreateAwbRequest request, Awb awb, HawbLockSettings hawbLockSettings, MawbLockSettings mawbLockSettings, Map<String, Boolean> awbGroupLocks) {
        if(!Boolean.TRUE.equals(awbGroupLocks.get(GOODS_DESCRIPTION_LOCK_GROUP))) {
            return;
        }
        List<AwbGoodsDescriptionInfo> awbGoodsDescriptionInfoList = new ArrayList<>();
        if (awb.getAwbGoodsDescriptionInfo() != null && !awb.getAwbGoodsDescriptionInfo().isEmpty())
            awbGoodsDescriptionInfoList = awb.getAwbGoodsDescriptionInfo().stream().filter(good -> good.getIsShipmentCreated() != null && good.getIsShipmentCreated()).toList();
        if (awb.getAwbGoodsDescriptionInfo() == null || awb.getAwbGoodsDescriptionInfo().isEmpty() || awbGoodsDescriptionInfoList.isEmpty()) {
            AwbGoodsDescriptionInfo awbGoodsDescriptionInfo = new AwbGoodsDescriptionInfo();
            Integer totalPacksCount = 0;
            awbGoodsDescriptionInfo.setIsShipmentCreated(true);
            awbGoodsDescriptionInfo.setEntityId(shipmentDetails.getId());
            awbGoodsDescriptionInfo.setEntityType(request.getAwbType());
            awbGoodsDescriptionInfo.setGrossWt(shipmentDetails.getWeight());
            awbGoodsDescriptionInfo.setGrossWtUnit(shipmentDetails.getWeightUnit());
            awbGoodsDescriptionInfo.setGrossVolume(shipmentDetails.getVolume() != null ? shipmentDetails.getVolume().setScale(3, RoundingMode.HALF_UP) : BigDecimal.ZERO.setScale(3, RoundingMode.HALF_UP));
            awbGoodsDescriptionInfo.setGrossVolumeUnit(shipmentDetails.getVolumeUnit());
            awbGoodsDescriptionInfo.setChargeableWt(shipmentDetails.getChargable() != null ?
                    AwbUtility.roundOffAirShipment(shipmentDetails.getChargable().doubleValue()) : null);
            awbGoodsDescriptionInfo.setGuid(UUID.randomUUID());
            awbGoodsDescriptionInfo.setNtrQtyGoods(shipmentDetails.getGoodsDescription() != null ? shipmentDetails.getGoodsDescription() : "");
            totalPacksCount = getPacksCountForPackingInfo(awb, awbGoodsDescriptionInfo, totalPacksCount);
            awbGoodsDescriptionInfo.setPiecesNo(totalPacksCount);
            if (awb.getAwbGoodsDescriptionInfo() == null) {
                awb.setAwbGoodsDescriptionInfo(new ArrayList<>());
            }
            awb.getAwbGoodsDescriptionInfo().add(awbGoodsDescriptionInfo);
        } else {
            processGoodsDescriptionFieldsLockedOrNot(shipmentDetails, request, awb, hawbLockSettings, mawbLockSettings, awbGoodsDescriptionInfoList);
        }
    }

    private Integer getPacksCountForPackingInfo(Awb awb, AwbGoodsDescriptionInfo awbGoodsDescriptionInfo, Integer totalPacksCount) {
        if (awb.getAwbPackingInfo() != null) {
            for (var awbPacking : awb.getAwbPackingInfo()) {
                if (awbPacking.getGuid() != null && awbPacking.getAwbGoodsDescriptionInfoGuid() == null) {
                    awbPacking.setAwbGoodsDescriptionInfoGuid(awbGoodsDescriptionInfo.getGuid());
                    totalPacksCount += Integer.parseInt(awbPacking.getPacks());
                    if (awbGoodsDescriptionInfo.getAwbPackingInfo() == null) {
                        awbGoodsDescriptionInfo.setAwbPackingInfo(new ArrayList<>());
                    }
                    awbGoodsDescriptionInfo.getAwbPackingInfo().add(awbPacking);
                }
            }
        }
        return totalPacksCount;
    }

    private void processGoodsDescriptionFieldsLockedOrNot(ShipmentDetails shipmentDetails, CreateAwbRequest request, Awb awb, HawbLockSettings hawbLockSettings, MawbLockSettings mawbLockSettings, List<AwbGoodsDescriptionInfo> awbGoodsDescriptionInfoList) {
        AwbGoodsDescriptionInfo awbGoodsDescriptionInfo = awbGoodsDescriptionInfoList.get(0);
        Integer totalPacksCount = 0;
        awbGoodsDescriptionInfo.setGrossWt(shipmentDetails.getWeight());
        awbGoodsDescriptionInfo.setGrossWtUnit(shipmentDetails.getWeightUnit());
        awbGoodsDescriptionInfo.setChargeableWt(shipmentDetails.getChargable() != null ? AwbUtility.roundOffAirShipment(shipmentDetails.getChargable().doubleValue()) : null);
        awbGoodsDescriptionInfo.setGrossVolume(shipmentDetails.getVolume() != null ? shipmentDetails.getVolume().setScale(3, RoundingMode.HALF_UP) : BigDecimal.ZERO.setScale(3, RoundingMode.HALF_UP));
        awbGoodsDescriptionInfo.setGrossVolumeUnit(shipmentDetails.getVolumeUnit());
        setTotalPacksCountForAwbPackingInfo(request, awb, hawbLockSettings, mawbLockSettings, awbGoodsDescriptionInfo, totalPacksCount);
    }

    private void setTotalPacksCountForAwbPackingInfo(CreateAwbRequest request, Awb awb, HawbLockSettings hawbLockSettings, MawbLockSettings mawbLockSettings, AwbGoodsDescriptionInfo awbGoodsDescriptionInfo, Integer totalPacksCount) {
        if (awb.getAwbPackingInfo() != null) {
            for (var awbPacking : awb.getAwbPackingInfo()) {
                if (Objects.equals(awbPacking.getAwbGoodsDescriptionInfoGuid(), awbGoodsDescriptionInfo.getGuid()))
                    totalPacksCount += Integer.parseInt(awbPacking.getPacks());
                if (awbPacking.getGuid() != null && awbPacking.getAwbGoodsDescriptionInfoGuid() == null) {
                    awbPacking.setAwbGoodsDescriptionInfoGuid(awbGoodsDescriptionInfo.getGuid());
                    totalPacksCount += Integer.parseInt(awbPacking.getPacks());
                    if (awbGoodsDescriptionInfo.getAwbPackingInfo() == null) {
                        awbGoodsDescriptionInfo.setAwbPackingInfo(new ArrayList<>());
                    }
                    awbGoodsDescriptionInfo.getAwbPackingInfo().add(awbPacking);
                }
            }
        }
        awbGoodsDescriptionInfo.setPiecesNo(totalPacksCount);
    }

    private void updateAwbCargoInfoFromShipment(ShipmentDetails shipmentDetails, CreateAwbRequest request, Awb awb, HawbLockSettings hawbLockSettings, MawbLockSettings mawbLockSettings, Map<String, Boolean> awbGroupLocks) {
        AwbCargoInfo awbCargoInfo = awb.getAwbCargoInfo();
        if (Boolean.TRUE.equals(awbGroupLocks.get(CARGO_DETAILS_LOCK_GROUP))) {
            awbCargoInfo.setAccountingInfo(awbCargoInfo.getAccountingInfo() == null ? null : awbCargoInfo.getAccountingInfo().toUpperCase());
        }
        if (Boolean.TRUE.equals(awbGroupLocks.get(GOODS_DESCRIPTION_LOCK_GROUP))) {
            GenerateAwbPaymentInfoRequest generateAwbPaymentInfoRequest = new GenerateAwbPaymentInfoRequest();
            generateAwbPaymentInfoRequest.setAwbCargoInfo(awbCargoInfo);
            generateAwbPaymentInfoRequest.setAwbPackingInfo(awb.getAwbPackingInfo());
            generateAwbPaymentInfoRequest.setAwbGoodsDescriptionInfo(awb.getAwbGoodsDescriptionInfo());
            generateAwbPaymentInfoRequest.setIsFromShipment(false);
            generateAwbPaymentInfoRequest.setPackUpdate(false);
        }
        if (Boolean.TRUE.equals(awbGroupLocks.get(CARGO_DETAILS_LOCK_GROUP))) {
            awbCargoInfo.setCustomsValue(BigDecimal.valueOf(0.0));
            awbCargoInfo.setCurrency(UserContext.getUser().getCompanyCurrency());
        }
        awbCargoInfo.setOtherInfo(awbCargoInfo.getOtherInfo() == null ? null : awbCargoInfo.getOtherInfo().toUpperCase());
        awbCargoInfo.setShippingInformation(awbCargoInfo.getShippingInformation() == null ? null : awbCargoInfo.getShippingInformation().toUpperCase());
        awbCargoInfo.setShippingInformationOther(awbCargoInfo.getShippingInformationOther() == null ? null : awbCargoInfo.getShippingInformationOther().toUpperCase());
        if(Boolean.TRUE.equals(awbGroupLocks.get(CARGO_DETAILS_LOCK_GROUP))) {
            updatChargeCodeCargoInfoAwb(shipmentDetails, request, hawbLockSettings, mawbLockSettings, awbCargoInfo);
        }
    }

    private void updatChargeCodeCargoInfoAwb(ShipmentDetails shipmentDetails, CreateAwbRequest request, HawbLockSettings hawbLockSettings, MawbLockSettings mawbLockSettings, AwbCargoInfo awbCargoInfo) {
        if(shipmentDetails.getIncoterms() != null && AwbConstants.incotermChargeCodeMap.containsKey(shipmentDetails.getIncoterms()))
            awbCargoInfo.setChargeCode(AwbConstants.incotermChargeCodeMap.get(shipmentDetails.getIncoterms()));
        else
            awbCargoInfo.setChargeCode(null);
    }

    private void updateAwbOtherInfoFromShipment(ShipmentDetails shipmentDetails, CreateAwbRequest request, Awb awb, HawbLockSettings hawbLockSettings, MawbLockSettings mawbLockSettings, Map<String, Boolean> awbGroupLocks) {
        if(Boolean.TRUE.equals(awbGroupLocks.get(OTHER_DETAILS_LOCK_GROUP))) {
            AwbOtherInfo awbOtherInfo = awb.getAwbOtherInfo();
            var shipperName = StringUtility.convertToString(shipmentDetails.getConsigner() != null ? shipmentDetails.getConsigner().getOrgData().get(PartiesConstants.FULLNAME) : "");
            awbOtherInfo.setShipper(shipperName == null ? null : shipperName.toUpperCase());
            awbOtherInfo.setExecutedOn(jsonHelper.convertValue(DateTimeFormatter.ofPattern(Constants.YYYY_MM_DD_T_HH_MM_SS).format(LocalDateTime.now()), LocalDateTime.class));
        }
    }

    @Override
    @Transactional
    public ResponseEntity<IRunnerResponse> partialAutoUpdateMawb(CommonRequestModel commonRequestModel) throws RunnerException {
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
        if (awbs.isEmpty()) {
            log.error("No Mawb exist for given Consolidation to update");
            throw new ValidationException("No Mawb exist for given Consolidation to update");
        }

        Awb awb = awbs.get(0);

        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();

        // fetch consolidation info
        ConsolidationDetails consolidationDetails = consolidationDetailsDao.findById(request.getConsolidationId()).get();
        if (shipmentSettingsDetails.getRestrictAWBEdit() != null && shipmentSettingsDetails.getRestrictAWBEdit()) {
            ResetAwbRequest resetAwbRequest = ResetAwbRequest.builder()
                    .id(awb.getId())
                    .shipmentId(request.getShipmentId())
                    .consolidationId(request.getConsolidationId())
                    .awbType(request.getAwbType())
                    .resetType(AwbReset.ALL)
                    .build();
            return resetAwb(CommonRequestModel.buildRequest(resetAwbRequest));
        } else if (Boolean.TRUE.equals(shipmentSettingsDetails.getAutoUpdateShipmentAWB())) {
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
        Set<Long> linkedHawbIds = mawbHawbLinks.stream().map(MawbHawbLink::getHawbId).collect(Collectors.toSet());
        if (consolidationDetails.getShipmentsList() != null && !consolidationDetails.getShipmentsList().isEmpty()) {
            for (var consoleShipment : consolidationDetails.getShipmentsList()) {
                if (consoleShipment.getId() != null) {
                    updateHawbMawb(mawbId, consoleShipment, linkedHawbIds);
                }
            }
        }
    }

    private void updateHawbMawb(Long mawbId, ShipmentDetails consoleShipment, Set<Long> linkedHawbIds) {
        List<Awb> awbList = awbDao.findByShipmentId(consoleShipment.getId());
        if (!CommonUtils.listIsNullOrEmpty(awbList)) {
            Optional<Awb> awbOptional = awbList.stream().findFirst();
            if (awbOptional.isPresent()) {
                Awb awb = awbOptional.get();
                if (awb != null && !linkedHawbIds.contains(awb.getId())) {
                    MawbHawbLink mawbHawblink = new MawbHawbLink();
                    mawbHawblink.setHawbId(awb.getId());
                    mawbHawblink.setMawbId(mawbId);
                    mawbHawbLinkDao.save(mawbHawblink);
                }
            }
        }
    }

    private void updateMawbFromShipment(CreateAwbRequest request, ConsolidationDetails consolidationDetails, Awb awb, ShipmentSettingsDetails shipmentSettingsDetails) {

        MawbLockSettings mawbLockSettings = shipmentSettingsDetails.getMawbLockSettings();
        attachedShipmentDescriptions = new ArrayList<>();
        totalVolumetricWeightOfAwbPacks = BigDecimal.ZERO;
        if (awb.getAwbPackingInfo() == null) {
            awb.setAwbPackingInfo(new ArrayList<>());
        }
        Map<String, Boolean> awbGroupLocks = AwbUtility.getGroupLockStatus(awb, null, mawbLockSettings);
        ArrayList<String> addressIds = collectAddressIds(consolidationDetails);
        Map<Long, AddressDataV1> addressDataV1Map = fetchAddressData(addressIds);
        awb.setAwbPackingInfo(updateMawbPackingInfoFromShipment(consolidationDetails, awbGroupLocks));
        updateMawbShipmentInfoFromShipment(consolidationDetails, awb, mawbLockSettings, addressDataV1Map, awbGroupLocks);
        generateMawbNotifyPartyinfo(consolidationDetails, request, awb, mawbLockSettings, awbGroupLocks);
        updateMawbRoutingInfoFromShipment(consolidationDetails, request, awb, mawbLockSettings, awbGroupLocks);
        updateMawbCargoInfoFromShipment(consolidationDetails, awb, mawbLockSettings, awbGroupLocks);
        generateMawbOtherInfo(consolidationDetails, awb, mawbLockSettings, awbGroupLocks);
    }

    private List<AwbPackingInfo> updateMawbPackingInfoFromShipment(ConsolidationDetails consolidationDetails, Map<String, Boolean> awbGroupLocks) {
        List<AwbPackingInfo> hawbPacksLinkedToMawb = new ArrayList<>();
        if(!Boolean.TRUE.equals(PACKING_LOCK_GROUP)) {
            return hawbPacksLinkedToMawb;
        }
        if (consolidationDetails.getShipmentsList() != null && !consolidationDetails.getShipmentsList().isEmpty()) {
            for (var consoleShipment : consolidationDetails.getShipmentsList()) {
                if (!StringUtility.isEmpty(consoleShipment.getGoodsDescription())) {
                    attachedShipmentDescriptions.add(consoleShipment.getGoodsDescription());
                }

                var awbList = awbDao.findByShipmentId(consoleShipment.getId());
                processAwbList(awbList, hawbPacksLinkedToMawb);
            }
            Double factor = Constants.AIR_FACTOR_FOR_VOL_WT;
            totalVolumetricWeightOfAwbPacks.multiply(BigDecimal.valueOf(factor)); //NOSONAR
        }
        return hawbPacksLinkedToMawb;
    }

    private void processAwbList(List<Awb> awbList, List<AwbPackingInfo> hawbPacksLinkedToMawb) {
        if (awbList != null && !awbList.isEmpty()) {
            Optional<Awb> awbOptional = awbList.stream().findFirst();
            if (awbOptional.isEmpty())
                return;
            var awb = awbOptional.get();
            if (awb.getAwbPackingInfo() != null && !awb.getAwbPackingInfo().isEmpty()) {
                for (var awbPack : awb.getAwbPackingInfo()) {
                    if (awbPack.getVolume() != null && !StringUtility.isEmpty(awbPack.getVolumeUnit()) &&
                            "M3".equals(awbPack.getVolumeUnit())) {
                        totalVolumetricWeightOfAwbPacks.add(awbPack.getVolume()); //NOSONAR
                    }
                    hawbPacksLinkedToMawb.add(awbPack);
                }
            }
        }
    }

    private void updateMawbShipmentInfoFromShipment(ConsolidationDetails consolidationDetails, Awb awb, MawbLockSettings mawbLockSettings, Map<Long,AddressDataV1> addressDataV1Map, Map<String, Boolean> awbGroupLocks) {
        AwbShipmentInfo awbShipmentInfo = awb.getAwbShipmentInfo();
        Map<String, String> alpha2DigitToCountry = masterDataUtils.consolidationAddressCountryMasterData(consolidationDetails);
        if (Boolean.TRUE.equals(awbGroupLocks.get(SHIPPER_LOCK_GROUP))) {
            var shipperName = StringUtility.convertToString(consolidationDetails.getSendingAgent().getOrgData().get(PartiesConstants.FULLNAME));
            awbShipmentInfo.setShipperName(shipperName == null ? shipperName : shipperName.toUpperCase());
            var shipperAddress = AwbUtility.constructAddressForAwb(consolidationDetails.getSendingAgent() != null ? consolidationDetails.getSendingAgent().getAddressData() : null);
            awbShipmentInfo.setShipperAddress(shipperAddress == null ? shipperAddress : shipperAddress.toUpperCase());
            if(consolidationDetails.getSendingAgent() != null && consolidationDetails.getSendingAgent().getAddressId() != null) {
                String shipperAddressId = consolidationDetails.getSendingAgent().getAddressId();
                constructShipperAddress(awbShipmentInfo, consolidationDetails.getSendingAgent() != null ? consolidationDetails.getSendingAgent().getAddressData() : null, alpha2DigitToCountry, addressDataV1Map.get(Long.valueOf(shipperAddressId)));
            }
        }
        if(Boolean.TRUE.equals(awbGroupLocks.get(CONSIGNEE_LOCK_GROUP))) {
            processReceivingAgentInMawbShipmentInfo(consolidationDetails, mawbLockSettings, awbShipmentInfo, alpha2DigitToCountry, addressDataV1Map);
        }
        // AwbUtility.getConsolidationForwarderDetails(uow, consolidationRow, awbShipmentInfo, awbOtherInfoRow, awbCargoInfo); LATER
        if (Boolean.TRUE.equals(awbGroupLocks.get(ROUTING_DETAILS_LOCK_GROUP))) {
            setAwbShipmentInfoUnLocationData(awbShipmentInfo, consolidationDetails.getCarrierDetails());
            awbShipmentInfo.setFirstCarrier(consolidationDetails.getCarrierDetails().getShippingLine());
        }
    }

    private void processReceivingAgentInMawbShipmentInfo(ConsolidationDetails consolidationDetails, MawbLockSettings mawbLockSettings, AwbShipmentInfo awbShipmentInfo, Map<String, String> alpha2DigitToCountry, Map<Long,AddressDataV1> addressDataV1Map) {
        var consigneeName = StringUtility.convertToString(consolidationDetails.getReceivingAgent() != null && consolidationDetails.getReceivingAgent().getOrgData() != null ? consolidationDetails.getReceivingAgent().getOrgData().get(PartiesConstants.FULLNAME) : "");
        awbShipmentInfo.setConsigneeName(consigneeName == null ? consigneeName : consigneeName.toUpperCase());
        if(consolidationDetails.getReceivingBranch() != null && consolidationDetails.getReceivingAgent().getAddressId() != null) {
            String consigneeAddressId = consolidationDetails.getReceivingAgent().getAddressId();
            constructConsigneeAddress(awbShipmentInfo, consolidationDetails.getReceivingAgent() != null ? consolidationDetails.getReceivingAgent().getAddressData() : null, alpha2DigitToCountry, addressDataV1Map.get(Long.valueOf(consigneeAddressId)));
            awbShipmentInfo.setConsigneeReferenceNumber(consolidationDetails.getReceivingAgent() != null ? consolidationDetails.getReceivingAgent().getId().toString() : null);
        }
    }

    private void generateMawbNotifyPartyinfo(ConsolidationDetails consolidationDetails, CreateAwbRequest request, Awb awb, MawbLockSettings mawbLockSettings, Map<String, Boolean> awbLockGroups) {
        if(!Boolean.TRUE.equals(awbLockGroups.get(NOTIFY_LOCK_GROUP))) {
            return;
        }
        Map<UUID, Parties> consolidationAddressMap = getConsolidationAddressMap(consolidationDetails);
        Map<String, String> alpha2DigitToCountry = masterDataUtils.consolidationAddressCountryMasterData(consolidationDetails);
        List<AwbNotifyPartyInfo> deleteAwbPartyList = new ArrayList<>();
        if (awb.getAwbNotifyPartyInfo() != null && !awb.getAwbNotifyPartyInfo().isEmpty()) {
            for (var awbParty : awb.getAwbNotifyPartyInfo()) {
                if (awbParty.getIsShipmentCreated() != null && awbParty.getIsShipmentCreated()) {
                    if (consolidationAddressMap.containsKey(awbParty.getGuid())) {
                        upadteConsolidationAddressMap(mawbLockSettings, awbParty, consolidationAddressMap, alpha2DigitToCountry);
                    } else {
                        deleteAwbPartyList.add(awbParty);
                    }
                }
            }
            awb.getAwbNotifyPartyInfo().removeAll(deleteAwbPartyList);
        }

        processConsolidationAddressMap(consolidationDetails, request, awb, consolidationAddressMap, alpha2DigitToCountry);

    }

    private void upadteConsolidationAddressMap(MawbLockSettings mawbLockSettings, AwbNotifyPartyInfo awbParty, Map<UUID, Parties> consolidationAddressMap, Map<String, String> alpha2DigitToCountry) {
        Parties party = consolidationAddressMap.get(awbParty.getGuid());
        ArrayList<String> notifyPartyIdList = new ArrayList<>();
        if(party.getAddressId() != null)
            notifyPartyIdList.add(party.getAddressId());
        Map<Long, AddressDataV1> addressDataV1Map = fetchAddressData(notifyPartyIdList);
        var name = StringUtility.convertToString(party.getOrgData().get(PartiesConstants.FULLNAME));
        awbParty.setName(name == null ? name : name.toUpperCase());
        if (party != null && party.getAddressId() != null && party.getAddressData() != null) {
            String partyAddressId = party.getAddressId();
            constructNotifyPartyAddress(awbParty, party.getAddressData(), alpha2DigitToCountry, addressDataV1Map.get(Long.valueOf(partyAddressId)));
        }
        consolidationAddressMap.remove(awbParty.getGuid());
    }

    private Map<UUID, Parties> getConsolidationAddressMap(ConsolidationDetails consolidationDetails) {
        Map<UUID, Parties> consolidationAddressMap = new HashMap<>();
        if (consolidationDetails.getConsolidationAddresses() != null &&
                !consolidationDetails.getConsolidationAddresses().isEmpty()) {
            for (var party : consolidationDetails.getConsolidationAddresses()) {
                if (party.getOrgData().get("Type") == "Notify Part 1" ||
                        party.getOrgData().get("Type") == "Notify Part 2" ||
                        party.getOrgData().get("Type") == "Notify Part 3") {
                    consolidationAddressMap.put(party.getGuid(), party);
                }
            }
        }
        return consolidationAddressMap;
    }

    private void processConsolidationAddressMap(ConsolidationDetails consolidationDetails, CreateAwbRequest request, Awb awb, Map<UUID, Parties> consolidationAddressMap, Map<String, String> alpha2DigitToCountry) {
        if (!consolidationAddressMap.isEmpty()) {
            for (var party : consolidationAddressMap.values()) {
                ArrayList<String> partyAddressList = new ArrayList<>();
                if(party.getAddressId() != null)
                    partyAddressList.add(party.getAddressId());
                Map<Long, AddressDataV1> addressDataV1Map = fetchAddressData(partyAddressList);
                AwbNotifyPartyInfo notifyPartyInfo = new AwbNotifyPartyInfo();
                var name = StringUtility.convertToString(party.getOrgData().get(PartiesConstants.FULLNAME));
                notifyPartyInfo.setName(name == null ? name : name.toUpperCase());
                if (party.getAddressData() != null) {
                    notifyPartyInfo.setAddress(AwbUtility.constructAddressForAwb(party.getAddressData()).toUpperCase());
                }
                notifyPartyInfo.setEntityId(consolidationDetails.getId());
                notifyPartyInfo.setEntityType(request.getAwbType());
                notifyPartyInfo.setIsShipmentCreated(true);
                notifyPartyInfo.setGuid(party.getGuid());
                if(party != null && party.getAddressId() != null) {
                    String partyAddressId = party.getAddressId();
                    constructNotifyPartyAddress(notifyPartyInfo, party.getAddressData(), alpha2DigitToCountry, addressDataV1Map.get(Long.valueOf(partyAddressId)));
                }
                // notifyPartyInfo.setAddressId(shipmentNotifyParty.getAddressData()); // field missing: AddressId
                notifyPartyInfo.setNotifyOrgId(consolidationDetails.getId());
                if (awb.getAwbNotifyPartyInfo() == null) {
                    awb.setAwbNotifyPartyInfo(new ArrayList<>());
                }
                awb.getAwbNotifyPartyInfo().add(notifyPartyInfo);
            }
        }
    }

    private void updateMawbRoutingInfoFromShipment(ConsolidationDetails consolidationDetails, CreateAwbRequest request, Awb awb, MawbLockSettings mawbLockSettings, Map<String, Boolean> awbGroupLocks) {
        if(!Boolean.TRUE.equals(awbGroupLocks.get(NEW_ROUTING_INFO_LOCK_GROUP))) {
            return;
        }
        boolean createRouting = true;
        createRouting = isCreateRouting(consolidationDetails, awb, mawbLockSettings, createRouting);

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
            if (awb.getAwbRoutingInfo() == null) {
                awb.setAwbRoutingInfo(new ArrayList<>());
            }
            awb.getAwbRoutingInfo().add(routingInfo);
        }
    }

    private boolean isCreateRouting(ConsolidationDetails consolidationDetails, Awb awb, MawbLockSettings mawbLockSettings, boolean createRouting) {
        AwbRoutingInfo deleteParty = new AwbRoutingInfo();
        if (awb.getAwbRoutingInfo() != null && !awb.getAwbRoutingInfo().isEmpty()) {
            for (var awbRoute : awb.getAwbRoutingInfo()) {
                if (isShipmentCreatedTrue(awbRoute)) {
                    createRouting = false;
                    if (consolidationDetails.getCarrierDetails() != null &&
                            consolidationDetails.getCarrierDetails().getOriginPort() != null &&
                            consolidationDetails.getCarrierDetails().getDestinationPort() != null) {
                        processMawbLockSettingsField(consolidationDetails, mawbLockSettings, awbRoute);
                    } else {
                        deleteParty = awbRoute;
                    }
                }
            }
            awb.getAwbRoutingInfo().remove(deleteParty);
        }
        return createRouting;
    }

    private void processMawbLockSettingsField(ConsolidationDetails consolidationDetails, MawbLockSettings mawbLockSettings, AwbRoutingInfo awbRoute) {
        awbRoute.setOriginPortName(consolidationDetails.getCarrierDetails().getOriginPort());
        awbRoute.setDestinationPortName(consolidationDetails.getCarrierDetails().getDestinationPort());
        awbRoute.setByCarrier(consolidationDetails.getCarrierDetails().getShippingLine());
        awbRoute.setFlightNumber(consolidationDetails.getCarrierDetails().getFlightNumber());
    }

    private void updateMawbCargoInfoFromShipment(ConsolidationDetails consolidationDetails, Awb awb, MawbLockSettings mawbLockSettings, Map<String, Boolean> awbGroupLocks) {
        AwbCargoInfo awbCargoInfo = awb.getAwbCargoInfo();
        if (Boolean.TRUE.equals(awbGroupLocks.get(GOODS_DESCRIPTION_LOCK_GROUP))) {
            GenerateAwbPaymentInfoRequest generateAwbPaymentInfoRequest = new GenerateAwbPaymentInfoRequest();
            generateAwbPaymentInfoRequest.setAwbCargoInfo(awbCargoInfo);
            generateAwbPaymentInfoRequest.setAwbPackingInfo(awb.getAwbPackingInfo());
            generateAwbPaymentInfoRequest.setAwbGoodsDescriptionInfo(awb.getAwbGoodsDescriptionInfo());
            generateAwbPaymentInfoRequest.setIsFromShipment(false);
            generateAwbPaymentInfoRequest.setPackUpdate(false);
        }

        if (Boolean.TRUE.equals(awbGroupLocks.get(CARGO_DETAILS_LOCK_GROUP))) {
            awbCargoInfo.setCustomsValue(BigDecimal.valueOf(0.0));
            awbCargoInfo.setCurrency(UserContext.getUser().getCompanyCurrency());
            awbCargoInfo.setHandlingInfo(getHandlingInfo(MasterDataType.MAWB_GENERATION, awb.getAwbPackingInfo(), consolidationDetails.getHazardous()));
            awbCargoInfo.setAccountingInfo(awbCargoInfo.getAccountingInfo() == null ? null : awbCargoInfo.getAccountingInfo().toUpperCase());
        }
        awbCargoInfo.setOtherInfo(awbCargoInfo.getOtherInfo() == null ? null : awbCargoInfo.getOtherInfo().toUpperCase());
        awbCargoInfo.setShippingInformation(awbCargoInfo.getShippingInformation() == null ? null : awbCargoInfo.getShippingInformation().toUpperCase());
        awbCargoInfo.setShippingInformationOther(awbCargoInfo.getShippingInformationOther() == null ? null : awbCargoInfo.getShippingInformationOther().toUpperCase());
        if (Boolean.TRUE.equals(awbGroupLocks.get(CARGO_DETAILS_LOCK_GROUP))) {
            updatChargeCodeCargoInfoMawb(consolidationDetails, mawbLockSettings, awbCargoInfo);
        }
    }

    private void updatChargeCodeCargoInfoMawb(ConsolidationDetails consolidationDetails, MawbLockSettings mawbLockSettings, AwbCargoInfo awbCargoInfo) {
        if(consolidationDetails.getIncoterms() != null && AwbConstants.incotermChargeCodeMap.containsKey(consolidationDetails.getIncoterms()))
            awbCargoInfo.setChargeCode(AwbConstants.incotermChargeCodeMap.get(consolidationDetails.getIncoterms()));
        else
            awbCargoInfo.setChargeCode(null);
    }

    private void generateMawbOtherInfo(ConsolidationDetails consolidationDetails, Awb awb, MawbLockSettings mawbLockSettings, Map<String, Boolean> awbGroupLocks) {
        if(!Boolean.TRUE.equals(awbGroupLocks.get(OTHER_DETAILS_LOCK_GROUP))) {
            return;
        }
        AwbOtherInfo awbOtherInfo = awb.getAwbOtherInfo();
        var shipperName = StringUtility.convertToString(consolidationDetails.getSendingAgent() != null ? consolidationDetails.getSendingAgent().getOrgData().get(PartiesConstants.FULLNAME) : "");
        awbOtherInfo.setShipper(shipperName == null ? null : shipperName.toUpperCase());
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
        CommonV1ListRequest masterDataRequest = CommonV1ListRequest.builder().skip(0).criteriaRequests(masterDataCriteria).build();
        V1DataResponse masterDataResponse = v1Service.fetchMasterData(masterDataRequest);
        if (masterDataResponse.getEntities() != null) {
            List<EntityTransferMasterLists> masterLists = jsonHelper.convertValueToList(masterDataResponse.entities, EntityTransferMasterLists.class);
            if (!masterLists.isEmpty())
                res = masterLists.get(0).getItemDescription();
        }
        res = getResForAirDgCase(awbPackingInfoList, dgFlag, res);
        return res;
    }

    private String getResForAirDgCase(List<AwbPackingInfo> awbPackingInfoList, Boolean dgFlag, String res) {
        if (Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getAirDGFlag()) && Boolean.TRUE.equals(dgFlag)) {
            Integer packs = getPacksCount(awbPackingInfoList);
            if (packs != 0) {
                if (!isStringNullOrEmpty(res))
                    res = res + "\n";
                else
                    res = "";
                res = res + "Dangerous Goods as per associated Shipper’s Declaration. " + packs.toString() + (packs > 1 ? " packages" : " package");
            }
        }
        return res;
    }

    private Integer getPacksCount(List<AwbPackingInfo> awbPackingInfoList) {
        Integer packs = 0;
        if (awbPackingInfoList != null && !awbPackingInfoList.isEmpty()) {
            for (AwbPackingInfo awbPackingInfo : awbPackingInfoList) {
                if (Boolean.TRUE.equals(awbPackingInfo.getHazardous()) && !isStringNullOrEmpty(awbPackingInfo.getPacks())) {
                    packs = packs + Integer.parseInt(awbPackingInfo.getPacks());
                }
            }
        }
        return packs;
    }

    @Override
    public ResponseEntity<IRunnerResponse> getAllMasterData(CommonRequestModel commonRequestModel, boolean isShipment) {
        String responseMsg;
        try {
            Long id = commonRequestModel.getId();
            List<Awb> optional = null;
            if (isShipment)
                optional = awbDao.findByShipmentId(id);
            else
                optional = awbDao.findByConsolidationId(id);
            if (optional == null || optional.isEmpty()) {
                log.debug("Shipment Details is null for Id {}", id);
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            Awb awb = optional.get(0);
            AwbResponse awbResponse = jsonHelper.convertValue(awb, AwbResponse.class);

            generateDefaultAwbInformation(awb, awbResponse);
            Map<String, Object> response = fetchAllMasterDataByKey(awbResponse);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_DATA_RETRIEVAL_FAILURE;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private Map<String, Object> fetchAllMasterDataByKey(AwbResponse awbResponse) {
        Map<String, Object> masterDataResponse = new HashMap<>();
        var masterListFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllMasterDataInSingleCall(awbResponse, masterDataResponse)), executorService);
        var unLocationsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllUnlocationDataInSingleCall(awbResponse, masterDataResponse)), executorService);
        var commodityTypesFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllCommodityTypesInSingleCallPacksList(awbResponse, masterDataResponse)), executorService);
        CompletableFuture.allOf(masterListFuture, unLocationsFuture, commodityTypesFuture).join();
        return masterDataResponse;
    }

    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllMasterDataInSingleCall(AwbResponse awbResponse, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            AtomicInteger count = new AtomicInteger();
            Set<MasterListRequest> listRequests = new HashSet<>(masterDataUtils.createInBulkMasterListRequest(awbResponse, Awb.class, fieldNameKeyMap, Awb.class.getSimpleName(), cacheMap));
            // Populate all the master data in inner objects
            if (!Objects.isNull(awbResponse.getAwbShipmentInfo()))
                listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(awbResponse.getAwbShipmentInfo(), AwbShipmentInfo.class, fieldNameKeyMap, AwbShipmentInfo.class.getSimpleName(), cacheMap));
            if (!Objects.isNull(awbResponse.getAwbRoutingInfo()))
                awbResponse.getAwbRoutingInfo().forEach(r -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(r, AwbRoutingInfo.class, fieldNameKeyMap, AwbRoutingInfo.class.getSimpleName() + count.incrementAndGet(), cacheMap)));
            if (!Objects.isNull(awbResponse.getAwbNotifyPartyInfo()))
                awbResponse.getAwbNotifyPartyInfo().forEach(n -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(n, AwbNotifyPartyInfo.class, fieldNameKeyMap, AwbNotifyPartyInfo.class.getSimpleName() + count.incrementAndGet(), cacheMap)));
            if (!Objects.isNull(awbResponse.getDefaultAwbShipmentInfo()))
                listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(awbResponse.getDefaultAwbShipmentInfo(), AwbShipmentInfo.class, fieldNameKeyMap, AwbShipmentInfo.class.getSimpleName() + count.incrementAndGet(), cacheMap));
            if (!Objects.isNull(awbResponse.getDefaultAwbRoutingInfo()))
                awbResponse.getDefaultAwbRoutingInfo().forEach(r -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(r, AwbRoutingInfo.class, fieldNameKeyMap, AwbRoutingInfo.class.getSimpleName() + count.incrementAndGet(), cacheMap)));
            if (!Objects.isNull(awbResponse.getAwbPackingInfo()))
                awbResponse.getAwbPackingInfo().forEach(r -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(r, AwbPackingInfo.class, fieldNameKeyMap, AwbPackingInfo.class.getSimpleName() + count.incrementAndGet(), cacheMap)));
            if (!Objects.isNull(awbResponse.getAwbGoodsDescriptionInfo()))
                awbResponse.getAwbGoodsDescriptionInfo().forEach(r -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(r, AwbGoodsDescriptionInfo.class, fieldNameKeyMap, AwbGoodsDescriptionInfo.class.getSimpleName() + count.incrementAndGet(), cacheMap)));
            if (!Objects.isNull(awbResponse.getAwbOtherChargesInfo()))
                awbResponse.getAwbOtherChargesInfo().forEach(r -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(r, AwbOtherChargesInfo.class, fieldNameKeyMap, AwbOtherChargesInfo.class.getSimpleName() + count.incrementAndGet(), cacheMap)));

            MasterListRequestV2 masterListRequestV2 = new MasterListRequestV2();
            masterListRequestV2.setMasterListRequests(listRequests.stream().toList());
            masterListRequestV2.setIncludeCols(Arrays.asList(MasterDataConstants.ITEM_TYPE, MasterDataConstants.ITEM_VALUE, "ItemDescription", "ValuenDesc", "Cascade"));

            Map<String, EntityTransferMasterLists> keyMasterDataMap = masterDataUtils.fetchInBulkMasterList(masterListRequestV2);
            Set<String> keys = new HashSet<>();
            commonUtils.createMasterDataKeysList(listRequests, keys);
            masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.MASTER_LIST, keys, new EntityTransferMasterLists(), cacheMap);

            if (masterDataResponse == null) {
                awbResponse.setMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(Awb.class.getSimpleName()), CacheConstants.MASTER_LIST, cacheMap));
                if (!Objects.isNull(awbResponse.getAwbShipmentInfo()))
                    awbResponse.getAwbShipmentInfo().setMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(AwbShipmentInfo.class.getSimpleName()), CacheConstants.MASTER_LIST, cacheMap));
            } else {
                masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.MASTER_LIST, masterDataResponse, cacheMap);
            }

            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(keyMasterDataMap));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occured in CompletableFuture: addAllMasterDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), AwbService.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }

    }

    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllUnlocationDataInSingleCall(AwbResponse awbResponse, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            AtomicInteger count = new AtomicInteger();
            Set<String> locationCodes = new HashSet<>();
            // Populate all the unlocation data in inner objects
            if (!Objects.isNull(awbResponse.getAwbShipmentInfo()))
                locationCodes.addAll((masterDataUtils.createInBulkUnLocationsRequest(awbResponse.getAwbShipmentInfo(), AwbShipmentInfo.class, fieldNameKeyMap, AwbShipmentInfo.class.getSimpleName(), cacheMap)));
            if (!Objects.isNull(awbResponse.getAwbOtherInfo()))
                locationCodes.addAll((masterDataUtils.createInBulkUnLocationsRequest(awbResponse.getAwbOtherInfo(), AwbOtherInfo.class, fieldNameKeyMap, AwbOtherInfo.class.getSimpleName(), cacheMap)));
            if (!Objects.isNull(awbResponse.getAwbRoutingInfo()))
                awbResponse.getAwbRoutingInfo().forEach(r -> locationCodes.addAll(masterDataUtils.createInBulkUnLocationsRequest(r, AwbRoutingInfo.class, fieldNameKeyMap, AwbRoutingInfo.class.getSimpleName() + (count.incrementAndGet()), cacheMap)));
            if (!Objects.isNull(awbResponse.getDefaultAwbShipmentInfo()))
                locationCodes.addAll((masterDataUtils.createInBulkUnLocationsRequest(awbResponse.getDefaultAwbShipmentInfo(), AwbShipmentInfo.class, fieldNameKeyMap, AwbShipmentInfo.class.getSimpleName() + Constants.class, cacheMap)));
            if (!Objects.isNull(awbResponse.getDefaultAwbRoutingInfo()))
                awbResponse.getDefaultAwbRoutingInfo().forEach(r -> locationCodes.addAll(masterDataUtils.createInBulkUnLocationsRequest(r, AwbRoutingInfo.class, fieldNameKeyMap, AwbRoutingInfo.class.getSimpleName() + (count.incrementAndGet()), cacheMap)));
            if (!Objects.isNull(awbResponse.getAwbPackingInfo()))
                awbResponse.getAwbPackingInfo().forEach(r -> locationCodes.addAll(masterDataUtils.createInBulkUnLocationsRequest(r, AwbPackingInfo.class, fieldNameKeyMap, AwbPackingInfo.class.getSimpleName() + (count.incrementAndGet()), cacheMap)));
            if (!Objects.isNull(awbResponse.getAwbGoodsDescriptionInfo()))
                awbResponse.getAwbGoodsDescriptionInfo().forEach(r -> locationCodes.addAll(masterDataUtils.createInBulkUnLocationsRequest(r, AwbGoodsDescriptionInfo.class, fieldNameKeyMap, AwbGoodsDescriptionInfo.class.getSimpleName() + (count.incrementAndGet()), cacheMap)));
            if (!Objects.isNull(awbResponse.getAwbNotifyPartyInfo()))
                awbResponse.getAwbNotifyPartyInfo().forEach(n -> locationCodes.addAll(masterDataUtils.createInBulkUnLocationsRequest(n, AwbNotifyPartyInfo.class, fieldNameKeyMap, AwbNotifyPartyInfo.class.getSimpleName() + (count.incrementAndGet()), cacheMap)));
            if (!Objects.isNull(awbResponse.getDefaultAwbNotifyPartyInfo()))
                awbResponse.getDefaultAwbNotifyPartyInfo().forEach(n -> locationCodes.addAll(masterDataUtils.createInBulkUnLocationsRequest(n, AwbNotifyPartyInfo.class, fieldNameKeyMap, AwbNotifyPartyInfo.class.getSimpleName() + (count.incrementAndGet()), cacheMap)));
            Map<String, EntityTransferUnLocations> keyMasterDataMap = masterDataUtils.fetchInBulkUnlocations(locationCodes, EntityTransferConstants.LOCATION_SERVICE_GUID);
            masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.UNLOCATIONS, locationCodes, new EntityTransferUnLocations(), cacheMap);

            if (masterDataResponse == null) {
                if (!Objects.isNull(awbResponse.getAwbShipmentInfo()))
                    awbResponse.getAwbShipmentInfo().setUnlocationData(masterDataUtils.setMasterData(fieldNameKeyMap.get(AwbShipmentInfo.class.getSimpleName()), CacheConstants.UNLOCATIONS_AWB, cacheMap));
            } else {
                masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.UNLOCATIONS_AWB, masterDataResponse, cacheMap);
            }

            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(keyMasterDataMap));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occured in CompletableFuture: addAllUnlocationDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), AwbService.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }

    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllCommodityTypesInSingleCallPacksList(AwbResponse awbResponse, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> commodityTypes = new HashSet<>();
            AtomicInteger count = new AtomicInteger();
            if (!Objects.isNull(awbResponse.getAwbPackingInfo()))
                awbResponse.getAwbPackingInfo().forEach(r -> commodityTypes.addAll(masterDataUtils.createInBulkCommodityTypeRequest(r, AwbPackingInfo.class, fieldNameKeyMap, AwbRoutingInfo.class.getSimpleName() + count.incrementAndGet(), cacheMap)));

            Map<String, EntityTransferCommodityType> v1Data = masterDataUtils.fetchInBulkCommodityTypes(commodityTypes.stream().toList());
            masterDataUtils.pushToCache(v1Data, CacheConstants.COMMODITY, commodityTypes, new EntityTransferCommodityType(), cacheMap);

            if (masterDataResponse == null) {
                if (!Objects.isNull(awbResponse.getAwbShipmentInfo()))
                    awbResponse.getAwbShipmentInfo().setCommodityMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(AwbShipmentInfo.class.getSimpleName()), CacheConstants.COMMODITY, cacheMap));
            } else {
                masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.COMMODITY, masterDataResponse, cacheMap);
            }

            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(v1Data));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occured in CompletableFuture: addAllCommodityTypesInSingleCallPacksList in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), AwbService.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }

    @SuppressWarnings("java:S2111")
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
        if (req.getAwbGoodsDescriptionInfo() != null) {
            for (var goods : req.getAwbGoodsDescriptionInfo()) {
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

        if (req.getAwbPaymentInfo() != null) {
            paymentInfo = req.getAwbPaymentInfo();
            paymentInfo.setWeightCharges(new BigDecimal(totalAmount));
            paymentInfo.setDueAgentCharges(agentOtherCharges != 0 ? new BigDecimal(agentOtherCharges) : null);
            paymentInfo.setDueCarrierCharges(carrierOtherCharges != 0 ? new BigDecimal(carrierOtherCharges) : null);
        }

        if (req.getChargeDetails() != null) {
            processRequestChargeDetails(req, totalAmount, agentOtherCharges, carrierOtherCharges, paymentInfo);
        }


        return ResponseHelper.buildSuccessResponse(AwbCalculationResponse.builder()
                .awbPackingInfo(req.getAwbPackingInfo()).awbGoodsDescriptionInfo(req.getAwbGoodsDescriptionInfo())
                .awbOtherChargesInfo(req.getAwbOtherChargesInfo()).awbPaymentInfo(paymentInfo).build());
    }

    private void processRequestChargeDetails(GenerateAwbPaymentInfoRequest req, double totalAmount, double agentOtherCharges, double carrierOtherCharges, AwbPaymentInfo paymentInfo) {
        double totalPrepaid;
        double totalCollect;

        double prepaidWeightCharge;
        double collectWeightCharge;
        double prepaidValuationCharge;
        double collectValuationCharge;
        double prepaidTax;
        double collectTax;
        double prepaidDueAgentCharges;
        double collectDueAgentCharges;
        double prepaidDueCarrierCharges;
        double collectDueCarrierCharges;

        if (!Objects.isNull(req.getChargeDetails().getIdentifier1()) && req.getChargeDetails().getIdentifier1().equals(Constants.TRUE)) {
            // Prepaid WeighCharges
            prepaidWeightCharge = totalAmount;
            prepaidValuationCharge = getDoubleValue(req.getAwbPaymentInfo().getValuationCharge());
            prepaidTax = getDoubleValue(req.getAwbPaymentInfo().getTax());
        } else {
            prepaidWeightCharge = 0.00;
            prepaidValuationCharge = 0.00;
            prepaidTax = 0.00;
        }

        if (!Objects.isNull(req.getChargeDetails().getIdentifier2()) && req.getChargeDetails().getIdentifier2().equals(Constants.TRUE)) {
            // CollectWeightCharges
            collectWeightCharge = totalAmount;
            collectValuationCharge = getDoubleValue(req.getAwbPaymentInfo().getValuationCharge());
            collectTax = getDoubleValue(req.getAwbPaymentInfo().getTax());
        } else {
            collectWeightCharge = 0.00;
            collectValuationCharge = 0.00;
            collectTax = 0.00;
        }

        if (!Objects.isNull(req.getChargeDetails().getIdentifier3()) && req.getChargeDetails().getIdentifier3().equals(Constants.TRUE)) {
            // PrepaidDueAgentCharges
            // PrepaidDueCarrierCharges
            prepaidDueAgentCharges = agentOtherCharges;
            prepaidDueCarrierCharges = carrierOtherCharges;
        } else {
            prepaidDueAgentCharges = 0.00;
            prepaidDueCarrierCharges = 0.00;
        }

        if (!Objects.isNull(req.getChargeDetails().getIdentifier4()) && req.getChargeDetails().getIdentifier4().equals(Constants.TRUE)) {
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

    private double calculateOtherCharges(GenerateAwbPaymentInfoRequest req, ChargesDue chargesDue) {
        double sum = 0.00;
        if (req.getAwbOtherChargesInfo() != null) {
            for (var otherCharges : req.getAwbOtherChargesInfo()) {
                if (otherCharges.getChargeDue() != null && chargesDue.equals(ChargesDue.getById(otherCharges.getChargeDue())))
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

            if (request.getIncludeColumns() == null || request.getIncludeColumns().isEmpty())
                return ResponseHelper.buildSuccessResponse(jsonHelper.convertValueToList(awb, AwbResponse.class));
            else {
                List<Object> data = new ArrayList<>();
                awb.forEach(a -> data.add(partialFetchUtils.fetchPartialListData(jsonHelper.convertValue(awb, AwbResponse.class), request.getIncludeColumns())));
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
        if (tenantSettingsList != null && !tenantSettingsList.isEmpty()) {
            tenantSettings = tenantSettingsList.get(0);
        }

        if (!CommonUtils.listIsNullOrEmpty(goodsDescriptionInfos)) {
            Map<UUID, List<AwbPackingInfo>> guidBasedAwbPackingList = getGuidBasedAwbPackingList(request, packsInfo, goodsDescriptionInfos);

            for (int i = 0; i < goodsDescriptionInfos.size(); i++) {
                var allPacks = guidBasedAwbPackingList.get(goodsDescriptionInfos.get(i).getGuid());
                Pair<BigDecimal, AwbGoodsDescriptionInfo> pair = calculateGoodsDescription(goodsDescriptionInfos.get(i), allPacks, tenantSettings, new HashMap<>(), request.isPackUpdate());
                totalVolumeticWeight = totalVolumeticWeight.add(pair.getLeft());
            }
        }
        return totalVolumeticWeight;
    }

    private Map<UUID, List<AwbPackingInfo>> getGuidBasedAwbPackingList(GenerateAwbPaymentInfoRequest request, List<AwbPackingInfo> packsInfo, List<AwbGoodsDescriptionInfo> goodsDescriptionInfos) {
        Map<UUID, List<AwbPackingInfo>> guidBasedAwbPackingList = new HashMap<>();
        if (!Objects.isNull(packsInfo)) {
            if (!Objects.isNull(request.getIsFromShipment()) && Boolean.TRUE.equals(request.getIsFromShipment()) && Objects.equals(goodsDescriptionInfos.get(0).getEntityType(), Constants.DMAWB)) {
                if (goodsDescriptionInfos.size() == 1) {
                    request.setPackUpdate(true);
                    guidBasedAwbPackingList.put(goodsDescriptionInfos.get(0).getGuid(), packsInfo);
                }
            } else if (!Objects.isNull(request.getIsFromShipment()) && Boolean.TRUE.equals(request.getIsFromShipment()))
                guidBasedAwbPackingList = packsInfo.stream().filter(c -> !Objects.isNull(c.getAwbGoodsDescriptionInfoGuid()))
                        .collect(Collectors.groupingBy(AwbPackingInfo::getAwbGoodsDescriptionInfoGuid));
            else
                guidBasedAwbPackingList.put(goodsDescriptionInfos.get(0).getGuid(), packsInfo);
        }
        return guidBasedAwbPackingList;
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
        String packsDescriptionValue = "";
        String dimensionText = Constants.DEFAULT_DIMN_TEXT;
        String newLine = "\r\n";

        if (request.getAwbPackingInfo() != null && !request.getAwbPackingInfo().isEmpty()) {
            packsDescriptionValue = getPacksDescriptionValue(request, packsDescriptionValue, newLine);
        } else {
            return "";
        }

        StringBuilder responseBuilder = new StringBuilder(StringUtility.isEmpty(dimensionText) ? Constants.EMPTY_STRING : dimensionText);
        responseBuilder.append(newLine).append(packsDescriptionValue);
        return responseBuilder.toString();
    }

    private String getPacksDescriptionValue(GenerateAwbPaymentInfoRequest request, String packsDescriptionValue, String newLine) {
        int counter = 0;
        StringBuilder packsDescriptionValueBuilder = new StringBuilder(packsDescriptionValue);
        for (AwbPackingInfo packings : request.getAwbPackingInfo()) {
            String pcs = " ";
            String len = " ";
            String width = " ";
            String height = " ";

            String cross = Constants.CROSS;

            if (packings.getPacks() != null) {
                pcs = packings.getPacks();
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

            packsDescriptionValueBuilder.append(len).append(width).append(height).append(" ").append(packings.getLengthUnit()).append(" ").append(cross).append(" ").append(pcs);
            if (counter < request.getAwbPackingInfo().size() - 1) {
                packsDescriptionValueBuilder.append(",").append(newLine);
            }
            counter++;
        }
        return packsDescriptionValueBuilder.toString();
    }

    @Override
    public ResponseEntity<IRunnerResponse> getChargeTypeMasterData(CommonGetRequest commonGetRequest) throws RunnerException {
        Long chargeTypeId = commonGetRequest.getId();
        if (chargeTypeId == null)
            throw new RunnerException("Please provide a valid Id");

        V1RetrieveRequest retrieveRequest = V1RetrieveRequest.builder().EntityId(String.valueOf(chargeTypeId)).build();
        V1RetrieveResponse v1DataResponse = v1Service.retrieveChargeCodeData(retrieveRequest);

        var chargeType = jsonHelper.convertValue(v1DataResponse.getEntity(), EntityTransferChargeType.class);
        var res = new AwbChargeTypeMasterDataResponse();
        for (var i : chargeType.getChargeTypeIntegrations()) {
            if (i.getIntegrationType().equals(ChargeTypeCode.IATA_Charge_Code)) {
                res.setIataDescription(i.getIntegrationCode());
            }
            if (i.getIntegrationType().equals(ChargeTypeCode.Due_To_Party)) {
                res.setChargeDue(i.getChargeDue());
            }
        }
        return ResponseHelper.buildSuccessResponse(res);
    }

    private String getFormattedAddress(EntityTransferOrganizations organization) {
        AwbAddressParam addressParam = AwbAddressParam.builder()
                .address1(organization.getAddress1())
                .address2(organization.getAddress2())
                .build();
        return AwbUtility.getFormattedAddress(addressParam);
    }

    private double getDoubleValue(BigDecimal number) {
        if (number == null)
            return 0.0;
        return number.doubleValue();
    }

    private void generateDefaultAwbOtherInfo(AwbResponse awbResponse) throws RunnerException {
        AwbOtherInfoResponse awbOtherInfoResponse = awbResponse.getAwbOtherInfo();
        try {
            if (awbResponse.getAwbShipmentInfo().getEntityType().equals(Constants.HAWB)) {
                CompanyDto companyDetails = fetchCompanyDetails();
                if (Strings.isNullOrEmpty(awbOtherInfoResponse.getBranch()))
                    awbOtherInfoResponse.setBranch(getTenantBranch());
                if (Strings.isNullOrEmpty(awbOtherInfoResponse.getLegalCompanyName()))
                    awbOtherInfoResponse.setLegalCompanyName(getLegalCompanyName());

                processCompanyDetails(companyDetails, awbOtherInfoResponse);
            } else {
                if (Strings.isNullOrEmpty(awbOtherInfoResponse.getCarrierName()))
                    awbOtherInfoResponse.setCarrierName(awbResponse.getAwbShipmentInfo().getFirstCarrier());
                if (Strings.isNullOrEmpty(awbOtherInfoResponse.getCarrierHqAddress()))
                    populateCarrierDetails(awbOtherInfoResponse.getCarrierName(), awbOtherInfoResponse, AwbOtherInfoResponse::setCarrierHqAddress);
            }
        } catch (Exception e) {
            throw new RunnerException(String.format("Error while populating default awb other info %s", e.getMessage()));
        }
    }

    private void processCompanyDetails(CompanyDto companyDetails, AwbOtherInfoResponse awbOtherInfoResponse) {
        if (companyDetails != null) {
            if (Strings.isNullOrEmpty(awbOtherInfoResponse.getAddress1()))
                awbOtherInfoResponse.setAddress1(companyDetails.getAddress1());
            if (Strings.isNullOrEmpty(awbOtherInfoResponse.getAddress2()))
                awbOtherInfoResponse.setAddress2(companyDetails.getAddress2());
            if (Strings.isNullOrEmpty(awbOtherInfoResponse.getState()))
                awbOtherInfoResponse.setState(companyDetails.getState());
            if (Strings.isNullOrEmpty(awbOtherInfoResponse.getCity()))
                awbOtherInfoResponse.setCity(companyDetails.getCity());
            if (Strings.isNullOrEmpty(awbOtherInfoResponse.getPincode()))
                awbOtherInfoResponse.setPincode(companyDetails.getZipPostCode());
            if (Strings.isNullOrEmpty(awbOtherInfoResponse.getCountryCode())) {
                setCountryDetailsForCompany(companyDetails, awbOtherInfoResponse);
            }
        }
    }

    private void setCountryDetailsForCompany(CompanyDto companyDetails, AwbOtherInfoResponse awbOtherInfoResponse) {
        String country = companyDetails.getCountry();
        List<String> alpha3CountriesList = new ArrayList<>();
        if (country != null) {
            if (country.length() == 2)
                country = CountryListHelper.ISO3166.getAlpha3FromAlpha2(country);
            if (country.length() == 3)
                alpha3CountriesList.add(country);
        }
        Map<String, String> alpha2DigitToCountry = masterDataUtils.getCountriesMasterListData(alpha3CountriesList);
        if (country != null && country.length() == 3)
            country = CountryListHelper.ISO3166.getAlpha2FromAlpha3(country);
        awbOtherInfoResponse.setCountryCode(country != null ? country : "");
        awbOtherInfoResponse.setCountryName(alpha2DigitToCountry != null ? alpha2DigitToCountry.get(country) : "");
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
            if (awbType.equalsIgnoreCase(MAWB)) {
                ConsolidationDetails consol = consolidationDetailsDao.findById(awb.getConsolidationId()).get();
                ArrayList<String> addressIdsList = collectAddressIds(consol);
                Map<Long, AddressDataV1> addressDataV1Map = fetchAddressData(addressIdsList);
                defaultAwbShipmentInfo = jsonHelper.convertValue(generateMawbShipmentInfo(consol, createAwbRequest, new AwbCargoInfo(), v1Service.retrieveTenant(), addressDataV1Map), AwbShipmentInfoResponse.class);
                defaultRoutingInfo = jsonHelper.convertValueToList(generateMawbRoutingInfo(consol, createAwbRequest), AwbRoutingInfoResponse.class);
                defaultNotifyPartyInfo = generateMawbNotifyPartyinfo(consol, createAwbRequest);
                awbResponse.setDefaultAwbShipmentInfo(defaultAwbShipmentInfo);
                awbResponse.setDefaultAwbNotifyPartyInfo(defaultNotifyPartyInfo);
                awbResponse.setDefaultAwbRoutingInfo(defaultRoutingInfo);
            } else {
                ShipmentDetails shipment = shipmentDao.findById(awb.getShipmentId()).get();
                ArrayList<String> addressIds = collectAddressIds(shipment);
                Map<Long, AddressDataV1> addressIdToEntityOrgMap = fetchAddressData(addressIds);
                defaultAwbShipmentInfo = jsonHelper.convertValue(generateAwbShipmentInfo(shipment, createAwbRequest, new AwbCargoInfo(), v1Service.retrieveTenant(), addressIdToEntityOrgMap), AwbShipmentInfoResponse.class);
                defaultRoutingInfo = jsonHelper.convertValueToList(generateAwbRoutingInfo(shipment, createAwbRequest), AwbRoutingInfoResponse.class);
                defaultNotifyPartyInfo = generateAwbNotifyPartyinfo(shipment, createAwbRequest, addressIdToEntityOrgMap);
                awbResponse.setDefaultAwbShipmentInfo(defaultAwbShipmentInfo);
                awbResponse.setDefaultAwbNotifyPartyInfo(defaultNotifyPartyInfo);
                awbResponse.setDefaultAwbRoutingInfo(defaultRoutingInfo);
                populateTaxRegistrationNumberInAwbResponse(awbResponse, shipment);
            }

        } catch (Exception e) {
            log.error("Error while creating default awbShipmentInfo object for awb having id {} with error \n {}", awb.getId(), e.getMessage());
        }

        return defaultAwbShipmentInfo;
    }

    private void setAwbShipmentInfoUnLocationData(AwbShipmentInfo awbShipmentInfo, CarrierDetails carrierDetails) {

        List<String> locationReferenceGuids = new ArrayList<>();
        locationReferenceGuids.add(carrierDetails.getOriginPort());
        locationReferenceGuids.add(carrierDetails.getDestinationPort());

        List<Object> criteria = Arrays.asList(
                Arrays.asList(EntityTransferConstants.LOCATION_SERVICE_GUID),
                "in",
                List.of(locationReferenceGuids)
        );
        CommonV1ListRequest commonV1ListRequest = CommonV1ListRequest.builder().skip(0).criteriaRequests(criteria).build();
        V1DataResponse v1DataResponse = v1Service.fetchUnlocation(commonV1ListRequest);

        List<EntityTransferUnLocations> locationDataList = jsonHelper.convertValueToList(v1DataResponse.entities, EntityTransferUnLocations.class);

        var locMap = locationDataList.stream().collect(Collectors.toMap(EntityTransferUnLocations::getLocationsReferenceGUID, EntityTransferUnLocations::getName, (oldValue, newValue) -> oldValue));
        awbShipmentInfo.setOriginAirport(locMap.get(carrierDetails.getOriginPort()));
        awbShipmentInfo.setDestinationAirport(locMap.get(carrierDetails.getDestinationPort()));
    }

    private void getAwbOtherInfoMasterData(AwbOtherInfo awbOtherInfo, String awbType) {
        MasterDataType masterDataType;
        if (awbType.equalsIgnoreCase("HAWB")) {
            masterDataType = MasterDataType.HAWB_CARRIER_AGENT;
        } else {
            masterDataType = MasterDataType.MAWB_CARRIER_AGENT;
        }
        List<Object> criteria = Arrays.asList(
                List.of(MasterDataConstants.ITEM_TYPE),
                "=",
                masterDataType.getId()
        );
        CommonV1ListRequest listRequest = CommonV1ListRequest.builder().skip(0).criteriaRequests(criteria).build();
        V1DataResponse v1DataResponse = v1Service.fetchMasterData(listRequest);
        List<EntityTransferMasterLists> entityTransferMasterList = jsonHelper.convertValueToList(v1DataResponse.entities, EntityTransferMasterLists.class);
        if (entityTransferMasterList != null && !entityTransferMasterList.isEmpty()) {
            awbOtherInfo.setCarrier(entityTransferMasterList.get(0).getItemValue());
        }

    }

    private String setUnLocationDataWithDiarcties(String name) {
        try {
            List<String> diarcties = new ArrayList<>();
            name = Optional.ofNullable(name).map(String::toLowerCase).orElse(Constants.EMPTY_STRING);
            diarcties.add(name);

            List<Object> criteria = Arrays.asList(
                    Arrays.asList(EntityTransferConstants.NAME_WO_DIACRITICS),
                    Operators.IN.getValue(),
                    List.of(diarcties)
            );
            CommonV1ListRequest commonV1ListRequest = CommonV1ListRequest.builder().skip(0).criteriaRequests(criteria).build();

            V1DataResponse v1DataResponse;
            v1DataResponse = v1Service.fetchUnlocation(commonV1ListRequest);
            List<EntityTransferUnLocations> locationDataList = jsonHelper.convertValueToList(v1DataResponse.entities, EntityTransferUnLocations.class);
            var locMap = locationDataList.stream().collect(Collectors.toMap(i -> StringUtility.convertToString(i.getNameWoDiacritics()).toLowerCase(), EntityTransferUnLocations::getLocationsReferenceGUID,
                    (locationguid1, locationguid2) -> locationguid1));
            return locMap.get(name);
        } catch (Exception e) {
            return Constants.EMPTY_STRING;
        }
    }

    private void populateTaxRegistrationNumberInAwbResponse(AwbResponse awbResponse, ShipmentDetails shipmentDetails) {
        // Step 1: Collect address IDs
        ArrayList<String> addressIdList = collectAddressIds(shipmentDetails);

        if (!CommonUtils.listIsNullOrEmpty(addressIdList)) {
            // Step 2: Fetch address data
            Map<Long, AddressDataV1> addressIdToEntityOrgMap = fetchAddressData(addressIdList);

            setTaxRegistrationForConsginer(shipmentDetails, addressIdToEntityOrgMap, awbResponse);
            setTaxRegistrationForConsginee(shipmentDetails, addressIdToEntityOrgMap, awbResponse);
            setTaxRegistrationForNotifyParty(shipmentDetails, addressIdToEntityOrgMap, awbResponse);
        }
    }

    /**
     * Collects address IDs from shipment details.
     */
    private ArrayList<String> collectAddressIds(ShipmentDetails shipmentDetails) {
        ArrayList<String> addressIdList = new ArrayList<>();

        addAddressIdIfPresent(addressIdList, shipmentDetails.getConsigner());
        addAddressIdIfPresent(addressIdList, shipmentDetails.getConsignee());

        if (shipmentDetails.getAdditionalDetails() != null && shipmentDetails.getAdditionalDetails().getNotifyParty() != null) {
            addAddressIdIfPresent(addressIdList, shipmentDetails.getAdditionalDetails().getNotifyParty());
        }

        return addressIdList;
    }

    /**
     * Collects address IDs from consolidation details.
     */
    private ArrayList<String> collectAddressIds(ConsolidationDetails consolidationDetails) {
        ArrayList<String> addressIdList = new ArrayList<>();

        addAddressIdIfPresent(addressIdList, consolidationDetails.getSendingAgent());
        addAddressIdIfPresent(addressIdList, consolidationDetails.getReceivingAgent());

        return addressIdList;
    }

    /**
     * Adds an address ID to the list if the party is not null and has an address ID.
     */
    private void addAddressIdIfPresent(List<String> addressIdList, Parties party) {
        if (party != null && party.getAddressId() != null) {
            addressIdList.add(party.getAddressId());
        }
    }

    /**
     * Fetches address data for the given address IDs.
     */
    private Map<Long, AddressDataV1> fetchAddressData(ArrayList<String> addressIdList) {
        if(!CommonUtils.listIsNullOrEmpty(addressIdList)) {
            CommonV1ListRequest addressRequest = createCriteriaToFetchAddressList(addressIdList);
            V1DataResponse addressResponse = v1Service.addressList(addressRequest);
            List<AddressDataV1> addressDataList = jsonHelper.convertValueToList(addressResponse.entities, AddressDataV1.class);

            return addressDataList.stream()
                    .collect(Collectors.toMap(AddressDataV1::getId, entity -> entity));
        }
        return new HashMap<>();
    }

    private void setTaxRegistrationForConsginer(ShipmentDetails shipmentDetails, Map<Long, AddressDataV1> addressIdToEntityOrgMap, AwbResponse awbResponse) {
        if (shipmentDetails.getConsigner() != null && shipmentDetails.getConsigner().getAddressId() != null) {
            Long consignerAddressId = Long.valueOf(shipmentDetails.getConsigner().getAddressId());
            if (addressIdToEntityOrgMap.containsKey(consignerAddressId)) {
                AddressDataV1 consignerAddressData = addressIdToEntityOrgMap.get(consignerAddressId);
                if (consignerAddressData != null) {
                    String consignerTaxRegNumber = consignerAddressData.getTaxRegNumber() != null
                            ? StringUtility.toUpperCase(StringUtility.convertToString(consignerAddressData.getTaxRegNumber()))
                            : null;
                    if (awbResponse.getDefaultAwbShipmentInfo() != null) {
                        awbResponse.getDefaultAwbShipmentInfo().setShipperTaxRegistrationNumber(consignerTaxRegNumber);
                    }
                }
            }
        }
    }

    private void setTaxRegistrationForConsginee(ShipmentDetails shipmentDetails, Map<Long, AddressDataV1> addressIdToEntityOrgMap, AwbResponse awbResponse) {
        if (shipmentDetails.getConsignee() != null && shipmentDetails.getConsignee().getAddressId() != null) {
            Long consigneeAddressId = Long.valueOf(shipmentDetails.getConsignee().getAddressId());
            if (addressIdToEntityOrgMap.containsKey(consigneeAddressId)) {
                AddressDataV1 consigneeAddressData = addressIdToEntityOrgMap.get(consigneeAddressId);
                if (consigneeAddressData != null) {
                    String consigneeTaxRegNumber = consigneeAddressData.getTaxRegNumber() != null
                            ? StringUtility.toUpperCase(StringUtility.convertToString(consigneeAddressData.getTaxRegNumber()))
                            : null;
                    if (awbResponse.getDefaultAwbShipmentInfo() != null) {
                        awbResponse.getDefaultAwbShipmentInfo().setConsigneeTaxRegistrationNumber(consigneeTaxRegNumber);
                    }
                }
            }
        }
    }

    private void setTaxRegistrationForNotifyParty(ShipmentDetails shipmentDetails, Map<Long, AddressDataV1> addressIdToEntityOrgMap, AwbResponse awbResponse) {
        Parties notifyParty = getNotifyParty(shipmentDetails);
        if (notifyParty == null || notifyParty.getAddressId() == null) {
            return;
        }

        Long notifyAddressId = Long.valueOf(notifyParty.getAddressId());
        AddressDataV1 notifyAddressData = addressIdToEntityOrgMap.get(notifyAddressId);
        if (notifyAddressData == null) {
            return;
        }

        String notifyTaxRegNumber = getFormattedTaxRegNumber(notifyAddressData);
        setTaxRegistrationNumber(awbResponse, notifyTaxRegNumber);
    }

    private Parties getNotifyParty(ShipmentDetails shipmentDetails) {
        if (shipmentDetails.getAdditionalDetails() == null) {
            return null;
        }
        return shipmentDetails.getAdditionalDetails().getNotifyParty();
    }

    private String getFormattedTaxRegNumber(AddressDataV1 notifyAddressData) {
        return notifyAddressData.getTaxRegNumber() != null
                ? StringUtility.toUpperCase(StringUtility.convertToString(notifyAddressData.getTaxRegNumber()))
                : null;
    }

    private void setTaxRegistrationNumber(AwbResponse awbResponse, String taxRegNumber) {
        List<AwbNotifyPartyInfo> notifyPartyInfoList = awbResponse.getDefaultAwbNotifyPartyInfo();
        if (notifyPartyInfoList != null && !notifyPartyInfoList.isEmpty()) {
            notifyPartyInfoList.get(0).setTaxRegistrationNumber(taxRegNumber);
        }
    }


    private void populateIssuingAgent(AwbShipmentInfo awbShipmentInfo, TenantModel tenantModel, AwbCargoInfo awbCargoInfo) throws RunnerException {
        if (tenantModel.DefaultOrgId == null) {
            return;
        }
        // Fetch Organization Data for defaultOrgId
        try {
            CommonV1ListRequest orgRequest = new CommonV1ListRequest();
            List<Object> orgField = new ArrayList<>(List.of("Id"));
            String operator = "=";
            List<Object> orgCriteria = new ArrayList<>(List.of(orgField, operator, tenantModel.DefaultOrgId));
            orgRequest.setCriteriaRequests(orgCriteria);
            V1DataResponse orgResponse = v1Service.fetchOrganization(orgRequest);
            List<EntityTransferOrganizations> orgList = jsonHelper.convertValueToList(orgResponse.entities, EntityTransferOrganizations.class);
            if (!orgList.isEmpty()) {

                // fetch all address for default org
                List<AddressDataV1> addressList = getDefaultAddress(tenantModel);
                Map<Long, AddressDataV1> issuingAgentAddressIdToEntityMap = getIssuingAgentAddressIdToEntityMap(addressList);
                awbShipmentInfo.setIssuingAgentName(StringUtility.toUpperCase(orgList.get(0).getFullName()));
                Long defaultAddressId = tenantModel.getDefaultAddressId();

                if (issuingAgentAddressIdToEntityMap.containsKey(defaultAddressId)) {
                    awbShipmentInfo.setIssuingAgentTaxRegistrationNumber(issuingAgentAddressIdToEntityMap.get(defaultAddressId).getTaxRegNumber() != null ? StringUtility.toUpperCase(issuingAgentAddressIdToEntityMap.get(defaultAddressId).getTaxRegNumber()) : null);
                }
                awbShipmentInfo.setIataCode(getIataCode(awbShipmentInfo, orgList));
                awbShipmentInfo.setAgentCASSCode(getAgentCASSCode(awbShipmentInfo, orgList));
                List<String> alpha3CountriesList = new ArrayList<>();
                if (addressList.isEmpty()) {
                    awbShipmentInfo.setIssuingAgentAddress(StringUtility.toUpperCase(orgList.get(0).getAddress1()));
                    awbShipmentInfo.setIssuingAgentAddress2(StringUtility.toUpperCase(orgList.get(0).getAddress2()));
                    awbShipmentInfo.setIssuingAgentCity(StringUtility.convertToString(orgList.get(0).getCity()));
                    awbShipmentInfo.setIssuingAgentState(orgList.get(0).getState());
                    awbShipmentInfo.setIssuingAgentZipCode(StringUtility.convertToString(orgList.get(0).getZipPostCode()));
                    awbShipmentInfo.setIssuingAgentTaxRegistrationNumber(StringUtility.convertToString(orgList.get(0).getTaxRegistrationNumber()));
                    awbShipmentInfo.setIssuingAgentPhone(StringUtility.convertToString(orgList.get(0).getPhone()));
                    processCountryForAddressList(awbShipmentInfo, orgList, alpha3CountriesList);
                } else {
                    AddressDataV1 address = addressList.stream().findFirst().orElse(AddressDataV1.builder().build());
                    if (address != null) {
                        var addressMap = jsonHelper.convertJsonToMap(jsonHelper.convertToJson(address));
                        alpha3CountriesList = masterDataUtils.addAlpha3Country(addressMap, alpha3CountriesList);
                        Map<String, String> alpha2DigitToCountry = masterDataUtils.getCountriesMasterListData(alpha3CountriesList);
                        constructIssuingAgentAddress(awbShipmentInfo, addressMap, alpha2DigitToCountry, address);
                    }
                }
                setExecutedAtWithCargoInfo(awbCargoInfo, orgList, addressList.stream().findFirst().orElse(AddressDataV1.builder().build()));
            }

        } catch (Exception e) {
            throw new RunnerException(String.format("Failed to fetch organization data for default org : %s", tenantModel.DefaultOrgId));
        }
    }

    private String getAgentCASSCode(AwbShipmentInfo awbShipmentInfo, List<EntityTransferOrganizations> orgList) {
        return awbShipmentInfo.getAgentCASSCode() == null ?
                orgList.get(0).getAgentCASSCode() : awbShipmentInfo.getAgentCASSCode();
    }

    private String getIataCode(AwbShipmentInfo awbShipmentInfo, List<EntityTransferOrganizations> orgList) {
        return awbShipmentInfo.getIataCode() == null ? orgList.get(0).getAgentIATACode() : awbShipmentInfo.getIataCode();
    }

    private Map<Long, AddressDataV1> getIssuingAgentAddressIdToEntityMap(List<AddressDataV1> addressList) {
        return Optional.of(addressList.stream().collect(Collectors.toMap(AddressDataV1::getId, entity -> entity))).orElse(new HashMap<>());
    }

    private void setExecutedAtWithCargoInfo(AwbCargoInfo awbCargoInfo, List<EntityTransferOrganizations> orgList, AddressDataV1 address) {
        if (awbCargoInfo != null) {
            EntityTransferOrganizations org = orgList.get(0) != null ? orgList.get(0) : EntityTransferOrganizations.builder().build();
            awbCargoInfo.setCustomOriginCode(getCountryCode(org.getCountry()));
            // Set Executed At
            if (StringUtility.isNotEmpty(address.getCity()))
                executedAt = setUnLocationDataWithDiarcties(address.getCity());
            else if (StringUtility.isNotEmpty(org.getCity()))
                executedAt = setUnLocationDataWithDiarcties(org.getCity());
            else
                executedAt = null;
        }
    }

    private void processCountryForAddressList(AwbShipmentInfo awbShipmentInfo, List<EntityTransferOrganizations> orgList, List<String> alpha3CountriesList) {
        String country = orgList.get(0).getCountry();
        if (country != null) {
            if (country.length() == 2)
                country = CountryListHelper.ISO3166.getAlpha3FromAlpha2(country);
            if (country.length() == 3)
                alpha3CountriesList.add(country);
        }
        Map<String, String> alpha2DigitToCountry = masterDataUtils.getCountriesMasterListData(alpha3CountriesList);
        if (country != null && country.length() == 3)
            country = CountryListHelper.ISO3166.getAlpha2FromAlpha3(country);
        awbShipmentInfo.setIssuingAgentCountry(country);
        awbShipmentInfo.setIssuingAgentCountryName(alpha2DigitToCountry != null ? alpha2DigitToCountry.get(country) : null);
    }

    private List<AddressDataV1> getDefaultAddress(TenantModel tenantModel) {
        List<AddressDataV1> addressList = new ArrayList<>();
        if (tenantModel.getDefaultAddressId() != null) {
            CommonV1ListRequest addressRequest = new CommonV1ListRequest();
            List<Object> addressField = new ArrayList<>(List.of("Id"));
            List<Object> addressCriteria = new ArrayList<>(List.of(addressField, "=", tenantModel.getDefaultAddressId()));
            addressRequest.setCriteriaRequests(addressCriteria);
            V1DataResponse addressResponse = v1Service.addressList(addressRequest);
            addressList = jsonHelper.convertValueToList(addressResponse.entities, AddressDataV1.class);
        }
        return addressList;
    }

    public String validateAwb(Awb awb) throws RunnerException {
        List<String> errors = new ArrayList<>();
        String awbType = awb.getAwbShipmentInfo().getEntityType();
        if (Constants.MAWB.equalsIgnoreCase(awbType)) {
            boolean allHawbsGenerated = true;
            var id = awb.getConsolidationId();
            if (id == null) {
                throw new RunnerException("ID can't be null, please provide a valid input !");
            }

            List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByConsolidationId(id);
            List<Long> shipmentIdList = consoleShipmentMappings.stream().map(ConsoleShipmentMapping::getShipmentId).toList();
            List<MawbHawbLink> mawbHawbLinks = mawbHawbLinkDao.findByMawbId(awb.getId());
            Set<Long> shipmentAwbIdSet = mawbHawbLinks.stream().map(MawbHawbLink::getHawbId).collect(Collectors.toSet());
            // Check whether HAWB is generated for all the linked shipments
            allHawbsGenerated = isAllHawbsGenerated(shipmentIdList, allHawbsGenerated, shipmentAwbIdSet, errors);
            if (Boolean.FALSE.equals(awb.getAirMessageResubmitted()) && !Objects.isNull(awb.getAirMessageStatus()))
                errors.add(AwbConstants.RESUBMIT_FWB_VALIDATION);

            if (!allHawbsGenerated)
                throw new RunnerException(AwbConstants.GENERATE_HAWB_BEFORE_MAWB_EXCEPTION);
        } else {
            // For HAWB/DMAWB
            addAirMessagingErrors(awb, errors, awbType);
        }

        return !errors.isEmpty() ? errors.toString() : null;
    }

    private void addAirMessagingErrors(Awb awb, List<String> errors, String awbType) {
        if (Boolean.FALSE.equals(awb.getAirMessageResubmitted()) && !Objects.isNull(awb.getAirMessageStatus()))
            errors.add(Constants.DMAWB.equalsIgnoreCase(awbType) ? AwbConstants.RESUBMIT_FWB_VALIDATION : AwbConstants.RESUBMIT_FZB_VALIDATION);
    }

    @SuppressWarnings("java:S135")
    private boolean isAllHawbsGenerated(List<Long> shipmentIdList, boolean allHawbsGenerated, Set<Long> shipmentAwbIdSet, List<String> errors) {
        for (var shipmentId : shipmentIdList) {
            List<Awb> response = awbDao.findByShipmentIdList(Arrays.asList(shipmentId));
            if (Objects.isNull(response) || response.isEmpty()) {
                allHawbsGenerated = false;
                break;
            } else if (!shipmentAwbIdSet.contains(response.get(0).getId())) {
                errors.add("Additional Shipments have been attached, please reset data as required.");
                break;
            }
        }
        return allHawbsGenerated;
    }

    private String fetchChargeCodes(String paymentTerms) {
        if (paymentTerms == null)
            return null;
        String chargeCode = null;
        if (paymentTerms.equals(Constants.PREPAID_DESC)) {
            chargeCode = Constants.PREPAID_CODE;
        } else if (paymentTerms.equals(Constants.COLLECT_DESC)) {
            chargeCode = Constants.COLLECT_CODE;
        } else if (paymentTerms.equals(Constants.COLLECT_PREPAID_DESC_CODE)) {
            chargeCode = Constants.COLLECT_PREPAID_DESC_CODE;
        } else if (paymentTerms.equals(Constants.PREPAID_COLLECT_DESC_CODE)) {
            chargeCode = Constants.PREPAID_COLLECT_DESC_CODE;
        }
        return chargeCode;
    }

    @Override
    public ResponseEntity<IRunnerResponse> validateIataAgent(Boolean fromShipment, Optional<Long> consolidationId) {
        TenantModel tenantModel = jsonHelper.convertValue(v1Service.retrieveTenant().getEntity(), TenantModel.class);
        IataAgentResponse res;
        if (tenantModel.IATAAgent && !Strings.isNullOrEmpty(tenantModel.AgentIATACode)
                && !Strings.isNullOrEmpty(tenantModel.AgentCASSCode) && !Strings.isNullOrEmpty(tenantModel.PIMAAddress)) {
            if (Boolean.TRUE.equals(fromShipment)) {
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
        if (shipmentId.isPresent()) {
            awbList = awbDao.findByShipmentId(shipmentId.get());
        } else if (consolidaitonId.isPresent()) {
            awbList = awbDao.findByConsolidationId(consolidaitonId.get());
        } else {
            // Throw some error both can't be null
            // or send empty response object
            return ResponseHelper.buildSuccessResponse();
        }

        if (!Objects.isNull(awbList) && !awbList.isEmpty()) {
            masterAwb = awbList.get(0);
            entityType = masterAwb.getAwbShipmentInfo().getEntityType();
        }
        FnmStatusMessageResponse fnmStatusMessageResponse = new FnmStatusMessageResponse();

        // Fetch the logs for all Awb entities (master and its linked Hawb)
        // Update the response as per AirMessagingLogs
        switch (entityType) {
            case Constants.HAWB -> fnmAcknowledgementHawb(masterAwb, fnmStatusMessageResponse);
            case Constants.DMAWB -> fnmAcknowledgementMawb(masterAwb, fnmStatusMessageResponse);
            case Constants.MAWB -> fnmAcknowledgementMawb(masterAwb, fnmStatusMessageResponse);
            default -> log.debug(Constants.SWITCH_DEFAULT_CASE_MSG, entityType);
        }

        return ResponseHelper.buildSuccessResponse(fnmStatusMessageResponse);
    }

    private void fnmAcknowledgementHawb(Awb awb, FnmStatusMessageResponse fnmStatusMessageResponse) {
        Boolean fnmStatus = null;
        StringBuilder responseStatusMessage = new StringBuilder();

        var statusLog = airMessagingLogsService.getRecentLogForEntityGuid(awb.getGuid());
        if (statusLog == null)
            return;
        if (Objects.equals(statusLog.getStatus(), AirMessagingStatus.FAILED.name())) {
            fnmStatus = false;
            responseStatusMessage.append(String.format(AirMessagingLogsConstants.SHIPMENT_FNM_FAILURE_ERROR, statusLog.getErrorMessage()));
        }
        if (Objects.equals(statusLog.getStatus(), AirMessagingStatus.SUCCESS.name())) {
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
        int successHawbCount = getSuccessHawbCount(linkedHawb, failedShipmentHawbs);


        boolean failedMawb = Objects.equals(mawbStatusLog.getStatus(), AirMessagingStatus.FAILED.name());
        boolean failedHawb = !failedShipmentHawbs.isEmpty();

        if (Objects.equals(mawbStatusLog.getStatus(), AirMessagingStatus.SUCCESS.name())) {
            fnmStatus = true;
            if (awbType.equals(Constants.DMAWB))
                responseStatusMessage.append("FWB submission is accepted");
            else if (successHawbCount == linkedHawb.size())
                responseStatusMessage.append("FWB and FZB submissions are accepted");
        }

        // Cases to generate the error Log
        if (!failedMawb && !failedHawb) {
            log.info("failedMawb and failedHawb are false, continue the process.");
        }
        if (failedHawb) {
            fnmStatus = false;
            // if failedShipmentHawb size > 0 fetch ShipmentID for those shipments
            var shipmentDetailsPage = shipmentDao.findShipmentsByIds(failedShipmentHawbs.stream().collect(Collectors.toSet()));
            String shipmentNumbersString = "";
            if (!shipmentDetailsPage.isEmpty()) {
                List<String> shipmentNumbers = shipmentDetailsPage.stream().map(ShipmentDetails::getShipmentId).toList();
                shipmentNumbersString = String.join(" ", shipmentNumbers);
            }

            if (failedMawb) {
                responseStatusMessage.append(String.format(
                        AirMessagingLogsConstants.CONSOLIDATION_FNM_MAWB_FAILURE_HAWB_FAILURE_ERROR, shipmentNumbersString)
                );
            } else {
                responseStatusMessage.append(String.format(
                        AirMessagingLogsConstants.CONSOLIDATION_FNM_MAWB_SUCCESS_HAWB_FAILURE_ERROR, shipmentNumbersString)
                );
            }
        }
        // !failedHawb && failedMawb
        else if (!failedHawb && failedMawb) {
            fnmStatus = false;
            responseStatusMessage.append(String.format(
                    AirMessagingLogsConstants.CONSOLIDATION_FNM_MAWB_FAILURE_HAWB_SUCCESS_ERROR, mawbStatusLog.getErrorMessage())
            );
        }

        fnmStatusMessageResponse.setFnmStatus(fnmStatus);
        fnmStatusMessageResponse.setResponse(responseStatusMessage.toString());
    }

    private int getSuccessHawbCount(List<Awb> linkedHawb, List<Long> failedShipmentHawbs) {
        int successHawbCount = 0;

        for (var hawb : linkedHawb) {
            var statusLog = airMessagingLogsService.getRecentLogForEntityGuid(hawb.getGuid());

            if (statusLog == null) continue;

            if (Objects.equals(statusLog.getStatus(), AirMessagingStatus.FAILED.name())) {
                failedShipmentHawbs.add(hawb.getShipmentId());
            }
            if (Objects.equals(statusLog.getStatus(), AirMessagingStatus.SUCCESS.name())) {
                successHawbCount += 1;
            }
        }
        return successHawbCount;
    }

    @Override
    public ResponseEntity<IRunnerResponse> getFetchIataRates(CommonRequestModel commonRequestModel) throws RunnerException {
        IataFetchRateRequest iataFetchRateRequest = (IataFetchRateRequest) commonRequestModel.getData();
        List<String> emptyFieldsError = getEmptyFieldsError(iataFetchRateRequest);
        if (!emptyFieldsError.isEmpty()) {
            String errorString = String.join(", ", emptyFieldsError);
            throw new ValidationException("Please add " + errorString + " and retry");
        }

        Map<String, UnlocationsResponse> unlocationsMap = masterDataUtils.getLocationData(Set.of(iataFetchRateRequest.getOriginPort(), iataFetchRateRequest.getDestinationPort()));
        Map<String, EntityTransferCarrier> carriersMap = masterDataUtils.fetchInBulkCarriers(Set.of(iataFetchRateRequest.getFlightCarrier()));

        if (unlocationsMap.isEmpty() || carriersMap.isEmpty()) {
            throw new ValidationException("Invalid data fetched for location or carriers.");
        }
        String origin = unlocationsMap.containsKey(iataFetchRateRequest.getOriginPort()) ? unlocationsMap.get(iataFetchRateRequest.getOriginPort()).getIataCode() : null;
        String destination = unlocationsMap.containsKey(iataFetchRateRequest.getDestinationPort()) ? unlocationsMap.get(iataFetchRateRequest.getDestinationPort()).getIataCode() : null;
        String carrier = carriersMap.containsKey(iataFetchRateRequest.getFlightCarrier()) ? carriersMap.get(iataFetchRateRequest.getFlightCarrier()).IATACode : null;
        List<String> emptyIataError = getEmptyIataError(origin, destination, carrier);
        if (!emptyIataError.isEmpty()) {
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
        log.info("TactPayload : " + jsonHelper.convertToJson(tactBridgePayload));
        BridgeServiceResponse bridgeServiceResponse = (BridgeServiceResponse) bridgeServiceAdapter.requestTactResponse(CommonRequestModel.buildRequest(tactBridgePayload));
        if (isBridgeServiceResponseNotValid(bridgeServiceResponse)) {
            log.error("Getting error from Iata while fetching rates due to: " + jsonHelper.convertToJson(bridgeServiceResponse));
            throw new RunnerException("Getting error from Iata while fetching rates");
        }

        IataTactRatesApiResponse iataTactRatesApiResponse = jsonHelper.convertValue(bridgeServiceResponse.getPayload(), IataTactRatesApiResponse.class);
        if (isIataTactRatesApiResponseNotValid(iataTactRatesApiResponse)) {
            List<String> errors = iataTactRatesApiResponse.getErrors().stream().map(IataTactRatesApiResponse.Errors::getMessage).toList();
            throw new ValidationException(String.join("\r\n", errors));
        }

        if (iataTactRatesApiResponse != null && iataTactRatesApiResponse.getRates() != null && !iataTactRatesApiResponse.getRates().isEmpty()) {
            ResponseEntity<IRunnerResponse> iataFetchRateResponse = getIataFetchResponse(iataTactRatesApiResponse, iataFetchRateRequest);
            if (iataFetchRateResponse != null) return iataFetchRateResponse;
        }

        IataFetchRateResponse iataFetchRateResponse = IataFetchRateResponse.builder().error("IATA did not return any value - please add the rate/ rate class manually").build();
        return ResponseHelper.buildSuccessResponse(iataFetchRateResponse);
    }

    private ResponseEntity<IRunnerResponse> getIataFetchResponse(IataTactRatesApiResponse iataTactRatesApiResponse, IataFetchRateRequest iataFetchRateRequest) {
        IataTactRatesApiResponse.StandardCharge standardCharge = iataTactRatesApiResponse.getRates().get(0).getStandardCharge();
        if (standardCharge.getWeightBreak() != null && !standardCharge.getWeightBreak().isEmpty()) {
            Map<BigDecimal, BigDecimal> weightBreakMap = standardCharge.getWeightBreak().stream().collect(Collectors.toMap(IataTactRatesApiResponse.WeightBreak::getWeightMeasure, IataTactRatesApiResponse.WeightBreak::getCharge));
            List<BigDecimal> weightList = standardCharge.getWeightBreak().stream().map(IataTactRatesApiResponse.WeightBreak::getWeightMeasure).sorted().toList();
            BigDecimal minimumCharge = standardCharge.getMinimumCharge();
            BigDecimal normalCharge = standardCharge.getNormalCharge();
            BigDecimal minWeightValue = weightList.get(0);
            BigDecimal chargeableWeight = iataFetchRateRequest.getChargeableWeight();
            if (chargeableWeight.compareTo(minWeightValue) < 0) {
                BigDecimal rate = chargeableWeight.multiply(normalCharge);
                if (rate.compareTo(minimumCharge) < 0) {
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
            for (int i = 0; i < size; i++) {
                BigDecimal currentWeight = weightList.get(i);
                if (chargeableWeight.compareTo(currentWeight) >= 0 && (i == (size - 1) || chargeableWeight.compareTo(weightList.get(i + 1)) < 0)) {
                    IataFetchRateResponse iataFetchRateResponse = IataFetchRateResponse.builder()
                            .rateClass(RateClass.Q.getId())
                            .rateCharge(weightBreakMap.get(weightList.get(i)))
                            .build();
                    return ResponseHelper.buildSuccessResponse(iataFetchRateResponse);
                }
            }
        }
        return null;
    }

    private boolean isIataTactRatesApiResponseNotValid(IataTactRatesApiResponse iataTactRatesApiResponse) {
        return iataTactRatesApiResponse != null && iataTactRatesApiResponse.getResponseType() != null && Objects.equals(iataTactRatesApiResponse.getResponseType(), "validation-failed")
                && iataTactRatesApiResponse.getErrors() != null && !iataTactRatesApiResponse.getErrors().isEmpty();
    }

    private boolean isBridgeServiceResponseNotValid(BridgeServiceResponse bridgeServiceResponse) {
        return bridgeServiceResponse.getExtraResponseParams().containsKey(AwbConstants.SERVICE_HTTP_STATUS_CODE) && !Objects.equals(bridgeServiceResponse.getExtraResponseParams().get(AwbConstants.SERVICE_HTTP_STATUS_CODE).toString(), "200") &&
                !Objects.equals(bridgeServiceResponse.getExtraResponseParams().get(AwbConstants.SERVICE_HTTP_STATUS_CODE).toString(), "400");
    }

    private List<String> getEmptyIataError(String origin, String destination, String carrier) {
        List<String> emptyIataError = new ArrayList<>();
        if (origin == null || origin.isEmpty()) {
            emptyIataError.add("Origin Airport");
        }
        if (destination == null || destination.isEmpty()) {
            emptyIataError.add("Destination Airport");
        }
        if (carrier == null || carrier.isEmpty()) {
            emptyIataError.add("Fligt Carrier");
        }
        return emptyIataError;
    }

    private List<String> getEmptyFieldsError(IataFetchRateRequest iataFetchRateRequest) {
        List<String> emptyFieldsError = new ArrayList<>();
        if (iataFetchRateRequest.getFlightCarrier() == null || iataFetchRateRequest.getFlightCarrier().isEmpty()) {
            emptyFieldsError.add("Fligt Carrier");
        }
        if (iataFetchRateRequest.getChargeableWeight() == null) {
            emptyFieldsError.add("Chargeable Weight");
        }
        if (iataFetchRateRequest.getOriginPort() == null || iataFetchRateRequest.getOriginPort().isEmpty()) {
            emptyFieldsError.add("Origin Airport");
        }
        if (iataFetchRateRequest.getDestinationPort() == null || iataFetchRateRequest.getDestinationPort().isEmpty()) {
            emptyFieldsError.add("Destination Airport");
        }
        if (iataFetchRateRequest.getCurrency() == null || iataFetchRateRequest.getCurrency().isEmpty()) {
            emptyFieldsError.add("Currency(in Cargo Information)");
        }
        return emptyFieldsError;
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