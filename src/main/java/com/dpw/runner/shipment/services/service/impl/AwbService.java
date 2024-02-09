package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
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
import com.dpw.runner.shipment.services.dto.request.AwbRequest;
import com.dpw.runner.shipment.services.dto.request.CreateAwbRequest;
import com.dpw.runner.shipment.services.dto.request.ResetAwbRequest;
import com.dpw.runner.shipment.services.dto.request.awb.*;
import com.dpw.runner.shipment.services.dto.response.AwbCalculationResponse;
import com.dpw.runner.shipment.services.dto.response.AwbResponse;
import com.dpw.runner.shipment.services.dto.response.AwbRoutingInfoResponse;
import com.dpw.runner.shipment.services.dto.response.AwbShipmentInfoResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.AwbReset;
import com.dpw.runner.shipment.services.entity.enums.ChargesDue;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCommodityType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferMasterLists;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferOrganizations;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferUnLocations;
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
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.IAwbService;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentService;
import com.dpw.runner.shipment.services.service.interfaces.ISyncQueueService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.syncing.Entity.AwbRequestV2;
import com.dpw.runner.shipment.services.syncing.Entity.SaveStatus;
import com.dpw.runner.shipment.services.syncing.constants.SyncingConstants;
import com.dpw.runner.shipment.services.syncing.interfaces.IAwbSync;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSync;
import com.dpw.runner.shipment.services.utils.*;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.retry.support.RetryTemplate;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.*;
import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;
import static com.dpw.runner.shipment.services.utils.CommonUtils.stringValueOf;

@SuppressWarnings("ALL")
@Service
@Slf4j
public class AwbService implements IAwbService {

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
    @Lazy
    @Autowired
    private ISyncQueueService syncQueueService;
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
    private MasterDataKeyUtils masterDataKeyUtils;

    ExecutorService executorService = Executors.newFixedThreadPool(10);



    private Integer totalPacks = 0;
    private List<String> attachedShipmentDescriptions = new ArrayList<>();
    private BigDecimal totalVolumetricWeightOfAwbPacks = new BigDecimal(0);

    @Value("${v1service.url.base}${v1service.url.awbSync}")
    private String AWB_V1_SYNC_URL;

    private String iataCode;
    private String executedAt;

    public ResponseEntity<?> createAwb(CommonRequestModel commonRequestModel) {
        String responseMsg;
        CreateAwbRequest request = (CreateAwbRequest) commonRequestModel.getData();
        if (request == null) {
            log.debug("Request is empty for AWB Create for Request Id {}", LoggerHelper.getRequestIdFromMDC());
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
                log.error("Error performing sync on AWB entity, {}", e);
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

    public ResponseEntity<?> updateAwb(CommonRequestModel commonRequestModel) {
        String responseMsg;
        AwbRequest request = (AwbRequest) commonRequestModel.getData();
        if (request == null) {
            log.error("Request is empty for AWB update for Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

        if (request.getId() == null) {
            log.error("Request Id is null for AWB update for Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        long id = request.getId();
        Optional<Awb> oldEntity = awbDao.findById(id);
        if (!oldEntity.isPresent()) {
            log.debug("AWB is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }


        Awb awb = convertRequestToEntity(request);
        awb.setAwbNumber(awb.getAwbShipmentInfo().getAwbNumber());
        if(awb.getAwbPackingInfo() != null && awb.getAwbPackingInfo().size() > 0) {
            for(var i : awb.getAwbPackingInfo()) {
                i.setAwbNumber(awb.getAwbNumber());
            }
        }
        try {
            String oldEntityJsonString = jsonHelper.convertToJson(oldEntity.get());
            updateAwbOtherChargesInfo(awb.getAwbOtherChargesInfo());
            if(awb.getAwbShipmentInfo().getEntityType().equals(Constants.MAWB)) {
                List<AwbPackingInfo> awbPackingInfoList = awb.getAwbPackingInfo();
                awb.setAwbPackingInfo(null);
                updateAwbPacking(awb.getId(), awbPackingInfoList);
            }
            awb = awbDao.save(awb);

            try {
                callV1Sync(awb, SaveStatus.UPDATE);
            } catch (Exception e) {
                log.error("Error performing sync on AWB entity, {}", e);
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

    private void updateAwbPacking(long mawbId, List<AwbPackingInfo> awbPackingInfoList) {
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
            for(Awb awb : awbList) {
                awb.setAwbPackingInfo(dataMap.get(awb.getAwbNumber()));
                awbDao.save(awb);
            }
        }
    }

    public ResponseEntity<?> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
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
                List<ShipmentSettingsDetails> tenantSettingsList = shipmentSettingsDao.getSettingsByTenantIds(Arrays.asList(UserContext.getUser().TenantId));
                ShipmentSettingsDetails tenantSettings = null;
                if (tenantSettingsList != null && tenantSettingsList.size() >= 1) {
                    tenantSettings = tenantSettingsList.get(0);
                }
                for (Awb awb : awbList) {
                    if (awb.getAwbShipmentInfo().getEntityType().equals(Constants.MAWB)) {
                        List<Awb> linkedHawb = getLinkedAwbFromMawb(awb.getId());
                        List<AwbPackingInfo> linkedPacks = new ArrayList<>();
                        for (var hawb : linkedHawb) {
                            if(hawb.getAwbPackingInfo() != null) {
                                linkedPacks.addAll(hawb.getAwbPackingInfo());
                            }
                        }
                        awb.setAwbPackingInfo(linkedPacks);
                        if(awb.getAwbGoodsDescriptionInfo() != null && awb.getAwbGoodsDescriptionInfo().size() > 0) {
                            calculateGoodsDescription(awb.getAwbGoodsDescriptionInfo().get(0), linkedPacks, tenantSettings, new HashMap<>());
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
                    res.setData(PartialFetchUtils.fetchPartialListData(curr,request.getIncludeColumns()));
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

    public ResponseEntity<?> retrieveById(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for AWB retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.error("Request Id is null for AWB retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<Awb> awb = awbDao.findById(id);
            if (!awb.isPresent()) {
                log.debug("AWB is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
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
                    calculateGoodsDescription(awb.get().getAwbGoodsDescriptionInfo().get(0), linkedPacks, tenantSettings, new HashMap<>());
                }
            }
            AwbResponse response = convertEntityToDto(awb.get());

            if(request.getIncludeColumns()==null||request.getIncludeColumns().size()==0)
                return ResponseHelper.buildSuccessResponse(response);
            else return ResponseHelper.buildSuccessResponse(PartialFetchUtils.fetchPartialListData(response, request.getIncludeColumns()));

        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public ResponseEntity<?> createMawb(CommonRequestModel commonRequestModel) {
        String responseMsg;

        CreateAwbRequest request = (CreateAwbRequest) commonRequestModel.getData();
        if (request == null) {
            log.debug("Request is empty for MAWB Create for Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

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
                    List<Awb> optionalAwb = awbDao.findByShipmentId(consoleShipment.getId());
                    if (optionalAwb == null || optionalAwb.isEmpty()) {
                        throw new ValidationException("To Generate Mawb, Please create Hawb for all the shipments attached");
                    }
                    Awb linkAwb = optionalAwb.stream().findFirst().get();
                    awbList.add(linkAwb);
                    if(linkAwb.getAwbPackingInfo() != null) {
                        mawbPackingInfo.addAll(linkAwb.getAwbPackingInfo());
                    }
                }
            }

            // save awb details
            awb = awbDao.save(generateMawb(request, consolidationDetails, mawbPackingInfo));
            try {
                callV1Sync(awb, SaveStatus.CREATE);
            } catch (Exception e) {
                log.error("Error performing sync on AWB entity, {}", e);
            }

            // map mawb and hawb affter suuccessful save
            LinkHawbMawb(consolidationDetails, awb, awbList);
            log.info("MAWB created successfully for Id {} with Request Id {}", awb.getId(), LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(awb));
    }

    public ResponseEntity<?> updateGoodsAndPacksForMawb(CommonRequestModel commonRequestModel) {
        String responseMsg;
        CreateAwbRequest request = (CreateAwbRequest) commonRequestModel.getData();
        if (request == null) {
            log.debug("Request is empty for Update Goods And Packs For Mawb for Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

        if (request.getShipmentId() == null) {
            log.error("Shipment Id can't be null or empty in Update Goods And Packs For Mawb Request");
            throw new ValidationException("Shipment Id can't be null or empty in Update Goods And Packs For Mawb");
        }

        Awb awb = new Awb();
        try {
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

    private void updateGoodsAndPacks(CreateAwbRequest request) {
        Awb mawb = awbDao.findByConsolidationId(request.getConsolidationId()).get(0);
        List<Awb> linkedHawb = getLinkedAwbFromMawb(mawb.getId());
        List<AwbPackingInfo> allHawbPacks = new ArrayList<>();
        for(var i : linkedHawb) {
            allHawbPacks.addAll(i.getAwbPackingInfo());
        }

        // Get tenant settings
        var tenantSettingsList = shipmentSettingsDao.getSettingsByTenantIds(Arrays.asList(UserContext.getUser().TenantId));
        ShipmentSettingsDetails tenantSettings = null;
        if (tenantSettingsList != null && tenantSettingsList.size() >= 1) {
            tenantSettings = tenantSettingsList.get(0);
        }

        if(allHawbPacks.size() == 0 && tenantSettings != null && tenantSettings.getConsolidationLite() != true) {
            updateGoodsDescForMawb(mawb);
        } else if (allHawbPacks.size() > 0) {
            calculateAndUpdateGoodsPacksMawb(allHawbPacks, mawb,tenantSettings);
        }
    }

    private void updateGoodsDescForMawb(Awb mawb) {
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

    private void calculateAndUpdateGoodsPacksMawb(List<AwbPackingInfo> allHawbPacks, Awb mawb, ShipmentSettingsDetails tenantSettings) {
        AwbGoodsDescriptionInfo mawbGoodsDescriptionInfo = null;
        if (mawb.getAwbGoodsDescriptionInfo() != null && mawb.getAwbGoodsDescriptionInfo().size() > 0) {
            // V1 always fetches via FirstOrDefault method post repo list call
            mawbGoodsDescriptionInfo = mawb.getAwbGoodsDescriptionInfo().get(0);
            Map<String, List<AwbPackingInfo>> hawbPacksMap = new HashMap<>(); // map to store awbNumber -> packsList

            var pair = calculateGoodsDescription(mawbGoodsDescriptionInfo, allHawbPacks, tenantSettings, hawbPacksMap);

            // Can there be a scenario of multiple Goods information ?
            mawb.setAwbGoodsDescriptionInfo(List.of(pair.getRight()));
            saveHawbPacks(mawb, hawbPacksMap);
            awbDao.save(mawb);
        }
    }

    private Pair<BigDecimal, AwbGoodsDescriptionInfo> calculateGoodsDescription(AwbGoodsDescriptionInfo mawbGoodsDescriptionInfo, List<AwbPackingInfo> allHawbPacks, ShipmentSettingsDetails tenantSettings, Map<String, List<AwbPackingInfo>> hawbPacksMap) {
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
            mawbGoodsDescriptionInfo.setGrossWt(totalGrossWeightOfMawbGood);
            mawbGoodsDescriptionInfo.setGrossWtUnit(grossWeightUnit);
            mawbGoodsDescriptionInfo.setPiecesNo(noOfPacks);
            mawbGoodsDescriptionInfo.setChargeableWt(roundOffAirShipment(chargeableWeightOfMawbGood));
        }

        if (mawbGoodsDescriptionInfo.getRateCharge() != null && mawbGoodsDescriptionInfo.getRateClass() != null) {
            if (mawbGoodsDescriptionInfo.getRateClass() == 1)
                mawbGoodsDescriptionInfo.setTotalAmount(mawbGoodsDescriptionInfo.getRateCharge());
            else
                mawbGoodsDescriptionInfo.setTotalAmount(mawbGoodsDescriptionInfo.getRateCharge().multiply(roundOffAirShipment(chargeableWeightOfMawbGood)));
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
                        Arrays.asList("ItemType"),
                        "=",
                        "105"
                );
                List<Object> subCriteria2 = Arrays.asList(
                        Arrays.asList("ItemValue"),
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
            if(res.getAwbShipmentInfo().getEntityType().equals("MAWB"))
                res.setErrors(validateMawb(awbShipmentInfo));
            responseList.add(res);
        });
        return responseList;
    }

    private Awb convertRequestToEntity(AwbRequest request) {
        var awb =  jsonHelper.convertValue(request, Awb.class);
        if(request.getShcIdList() != null && request.getShcIdList().size() > 0) {
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

    private Awb generateMawb(CreateAwbRequest request, ConsolidationDetails consolidationDetails, List<AwbPackingInfo> awbPackingInfo) {

        if(request.getIsReset() == null || request.getIsReset() == false) {
            List<Awb> existingAwbs = awbDao.findByConsolidationId(request.getConsolidationId());
            if(existingAwbs.size() > 0)
                throw new RunnerException("MAWB already created for current Consolidation !");
        }

        // validate the request
        AwbUtility.validateConsolidationInfoBeforeGeneratingAwb(consolidationDetails);

        //var awbPackingInfo = generateMawbPackingInfo(consolidationDetails);
        // generate Awb Entity
        return Awb.builder()
                .awbNumber(consolidationDetails.getMawb())
                .awbShipmentInfo(generateMawbShipmentInfo(consolidationDetails, request))
                .awbNotifyPartyInfo(generateMawbNotifyPartyinfo(consolidationDetails, request))
                .awbRoutingInfo(generateMawbRoutingInfo(consolidationDetails, request))
                .awbGoodsDescriptionInfo(generateMawbGoodsDescriptionInfo(consolidationDetails, request, null))
                .awbCargoInfo(generateMawbCargoInfo(consolidationDetails, request, awbPackingInfo))
                .awbOtherInfo(generateMawbOtherInfo(consolidationDetails, request))
                //.awbPackingInfo(awbPackingInfo)
                .consolidationId(consolidationDetails.getId())
                .build();
    }

    private AwbShipmentInfo generateMawbShipmentInfo(ConsolidationDetails consolidationDetails, CreateAwbRequest request) {
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
        setAwbShipmentInfoUnLocationData(awbShipmentInfo, consolidationDetails.getCarrierDetails());
        setTenantFieldsInAwbShipmentInfo(awbShipmentInfo, tenantModel);

         for (var orgRow : consolidationDetails.getConsolidationAddresses()) {
            if (orgRow.getType().equals(Constants.FORWARDING_AGENT)) {
                var issuingAgentName = StringUtility.convertToString(orgRow.getOrgData().get(PartiesConstants.FULLNAME));
                awbShipmentInfo.setIssuingAgentName(issuingAgentName == null ? issuingAgentName : issuingAgentName.toUpperCase()); // extract from orgdata
                var issuingAgentAddress = AwbUtility.constructAddress(orgRow.getAddressData());
                awbShipmentInfo.setIssuingAgentAddress(issuingAgentAddress == null ? issuingAgentAddress : issuingAgentAddress.toUpperCase());

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
        populateIssuingAgent(awbShipmentInfo, tenantModel);
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
            var sortedRoutingList = consolidationDetails.getRoutingsList();
            List<AwbRoutingInfo> res = new ArrayList<>();
            Collections.sort(sortedRoutingList, Comparator.comparing(Routings::getLeg));
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
                res.add(awbRoutingInfo);
            }
            return res;
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
            return Arrays.asList(routingInfo);
        }
        return null;
    }

    private AwbCargoInfo generateMawbCargoInfo(ConsolidationDetails consolidationDetails, CreateAwbRequest request, List<AwbPackingInfo> awbPackingList) {
        AwbCargoInfo awbCargoInfo = new AwbCargoInfo();
        String concatenatedGoodsDesc = ""; //TODO from consoleshipment mapping
        if (attachedShipmentDescriptions.size() > 0) {
            concatenatedGoodsDesc = String.join(",", attachedShipmentDescriptions);
        }
        awbCargoInfo.setNtrQtyGoods(AwbUtility.generateNatureAndQuantFieldsForConsolMawb(concatenatedGoodsDesc, totalVolumetricWeightOfAwbPacks, awbPackingList));
        awbCargoInfo.setEntityId(consolidationDetails.getId());
        awbCargoInfo.setEntityType(request.getAwbType());
//        awbCargoInfo.setCarriageValue(shipmentDetails.getGoodsValue() != null ? shipmentDetails.getGoodsValue() : new BigDecimal(0.0)); // field missing
//        awbCargoInfo.setCarriageValue(shipmentDetails.getInsuranceValue() != null ? shipmentDetailsgetInsuranceValue() : new BigDecimal(0.0)); // field missing
        awbCargoInfo.setCustomsValue(new BigDecimal(0.0));
        awbCargoInfo.setCurrency(userContext.getUser().getCompanyCurrency());
        awbCargoInfo.setHandlingInfo(getHandlingInfo(MasterDataType.MAWB_GENERATION));
        awbCargoInfo.setAccountingInfo(awbCargoInfo.getAccountingInfo() == null ? null : awbCargoInfo.getAccountingInfo().toUpperCase());
        awbCargoInfo.setOtherInfo(awbCargoInfo.getOtherInfo() == null ? null : awbCargoInfo.getOtherInfo().toUpperCase());
        awbCargoInfo.setNtrQtyGoods(awbCargoInfo.getNtrQtyGoods() == null ? null : awbCargoInfo.getNtrQtyGoods().toUpperCase());
        awbCargoInfo.setShippingInformation(awbCargoInfo.getShippingInformation() == null ? null : awbCargoInfo.getShippingInformation().toUpperCase());
        awbCargoInfo.setShippingInformationOther(awbCargoInfo.getShippingInformationOther() == null ? null : awbCargoInfo.getShippingInformationOther().toUpperCase());
        var consolOptional = consolidationDetailsDao.findById(request.getConsolidationId());
        if (consolOptional.isPresent()) {
            var consol = consolOptional.get();
            for (var consolAddressRow : consol.getConsolidationAddresses()) {
                if (consolAddressRow.getType() != null && consolAddressRow.getType().equals(Constants.FORWARDING_AGENT)) {
                    String country = consolAddressRow.getAddressData() != null ?
                            (String) consolAddressRow.getAddressData().get(COUNTRY) : null;
                    if (country != null)
                        awbCargoInfo.setCustomOriginCode(getCountryCode(country));
                }
            }
        }
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
        awbOtherInfo.setExecutedOn(jsonHelper.convertValue(DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss").format(LocalDateTime.now()), LocalDateTime.class));
        awbOtherInfo.setExecutedAt(executedAt);
        getAwbOtherInfoMasterData(awbOtherInfo, request.getAwbType());
        return awbOtherInfo;
    }

    private void LinkHawbMawb(ConsolidationDetails consolidationDetails, Awb mawb, List<Awb> awbList) {
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

    private List<AwbPackingInfo> generateMawbPackingInfo(ConsolidationDetails consolidationDetails) {
        List<AwbPackingInfo> awbPackingList = new ArrayList<>();
        List<AwbGoodsDescriptionInfo> awbGoodsDescList = new ArrayList<>();
        List<Long> attachedHawbIds = new ArrayList<>();
        List<AwbPackingInfo> hawbPacksLinkedToMawb = new ArrayList<>();

        if (consolidationDetails.getShipmentsList().size() > 0) {
            for (var consoleShipment : consolidationDetails.getShipmentsList()) {
                if (!StringUtility.isEmpty(consoleShipment.getGoodsDescription())) {
                    attachedShipmentDescriptions.add(consoleShipment.getGoodsDescription());
                }

                var awbList = awbDao.findByShipmentId(consoleShipment.getId());
                if (awbList == null || awbList.size() == 0) {
                    throw new ValidationException("To Generate Mawb, Please create Hawb for all the shipments attached");
                }

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
            Double factor = Constants.FACTOR_VOL_WT;
            totalVolumetricWeightOfAwbPacks.multiply(new BigDecimal(factor));
        }
        return hawbPacksLinkedToMawb;
    }

    private Awb generateAwb(CreateAwbRequest request) {

        if(request.getIsReset() == null || request.getIsReset() == false) {
            List<Awb> existingAwbs = awbDao.findByShipmentId(request.getShipmentId());
            if(existingAwbs.size() > 0)
                throw new RunnerException("AWB already created for current Shipment !");
        }

        // fetch sehipment info
        ShipmentDetails shipmentDetails = shipmentDao.findById(request.getShipmentId()).get();
        boolean syncShipment = false;
        if(shipmentDetails.getHouseBill() == null) {
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
        // generate Awb Entity
        return Awb.builder()
                .awbNumber(shipmentDetails.getHouseBill())
                .awbShipmentInfo(generateAwbShipmentInfo(shipmentDetails, request))
                .awbNotifyPartyInfo(generateAwbNotifyPartyinfo(shipmentDetails, request))
                .awbRoutingInfo(generateAwbRoutingInfo(shipmentDetails, request))
                .awbGoodsDescriptionInfo(generateAwbGoodsDescriptionInfo(shipmentDetails, request, awbPackingInfo))
                .awbCargoInfo(generateAwbCargoInfo(shipmentDetails, request, awbPackingInfo))
                .awbOtherInfo(generateAwbOtherInfo(shipmentDetails, request))
                .awbPackingInfo(awbPackingInfo)
                .shipmentId(shipmentDetails.getId())
                .build();
    }

    private AwbShipmentInfo generateAwbShipmentInfo(ShipmentDetails shipmentDetails, CreateAwbRequest request) {
        AwbShipmentInfo awbShipmentInfo = new AwbShipmentInfo();
        awbShipmentInfo.setEntityId(shipmentDetails.getId());
        awbShipmentInfo.setEntityType(request.getAwbType());
        awbShipmentInfo.setAwbNumber(shipmentDetails.getHouseBill());
        var shipperName = StringUtility.convertToString(shipmentDetails.getConsigner() != null && shipmentDetails.getConsigner().getOrgData() != null ? shipmentDetails.getConsigner().getOrgData().get(PartiesConstants.FULLNAME): "");
        awbShipmentInfo.setShipperName(shipperName == null ? shipperName : shipperName.toUpperCase());
        var shipperAddress = AwbUtility.constructAddress(shipmentDetails.getConsigner() != null ? shipmentDetails.getConsigner().getAddressData() : null);
        awbShipmentInfo.setShipperAddress(shipperAddress == null ? shipperAddress : shipperAddress.toUpperCase());
        var consigneeName = StringUtility.convertToString(shipmentDetails.getConsignee() != null && shipmentDetails.getConsignee().getOrgData() != null? shipmentDetails.getConsignee().getOrgData().get(PartiesConstants.FULLNAME) : "");
        awbShipmentInfo.setConsigneeName(consigneeName == null ? consigneeName : consigneeName.toUpperCase());
        var consigneeAddress = AwbUtility.constructAddress(shipmentDetails.getConsignee() != null ? shipmentDetails.getConsignee().getAddressData() : null);
        awbShipmentInfo.setConsigneeAddress(consigneeAddress == null ? consigneeAddress : consigneeAddress.toUpperCase());

        awbShipmentInfo.setConsigneeReferenceNumber(shipmentDetails.getConsignee() != null ? shipmentDetails.getConsignee().getId().toString() : null);
//        awbShipmentInfo.setOriginAirport(shipmentDetails.getCarrierDetails() != null ? shipmentDetails.getCarrierDetails().getOriginPort() : null);
//        awbShipmentInfo.setDestinationAirport(shipmentDetails.getCarrierDetails() != null ? shipmentDetails.getCarrierDetails().getDestinationPort() : null);
        setAwbShipmentInfoUnLocationData(awbShipmentInfo,shipmentDetails.getCarrierDetails());

        awbShipmentInfo.setFirstCarrier(shipmentDetails.getCarrierDetails() != null ? shipmentDetails.getCarrierDetails().getShippingLine() : null);

        for (var orgRow : shipmentDetails.getShipmentAddresses()) {
            if (orgRow.getType().equals(Constants.FORWARDING_AGENT)) {
                var issuingAgentName = StringUtility.convertToString(orgRow.getOrgData().get(PartiesConstants.FULLNAME));
                awbShipmentInfo.setIssuingAgentName(issuingAgentName == null ? issuingAgentName : issuingAgentName.toUpperCase()); // extract from orgdata
                var issuingAgentAddress = AwbUtility.constructAddress(orgRow.getAddressData());
                awbShipmentInfo.setIssuingAgentAddress(issuingAgentAddress == null ? issuingAgentAddress : issuingAgentAddress.toUpperCase());

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

        var consolidationDetails = consolidationDetailsDao.findById(request.getConsolidationId());
        if (consolidationDetails.isPresent()) {
            var consol = consolidationDetails.get();
            for (var orgRow : consol.getConsolidationAddresses()) {
                if (orgRow.getType().equals(ISSUING_CARRIER_AGENT_NAME)) {
                    var issuingAgentName = StringUtility.convertToString(orgRow.getOrgData().get(PartiesConstants.FULLNAME));
                    awbShipmentInfo.setIssuingAgentName(issuingAgentName == null ? issuingAgentName : issuingAgentName.toUpperCase()); // extract from orgdata
                    var issuingAgentAddress = AwbUtility.constructAddress(orgRow.getAddressData());
                    awbShipmentInfo.setIssuingAgentAddress(issuingAgentAddress == null ? issuingAgentAddress : issuingAgentAddress.toUpperCase());

                    awbShipmentInfo.setIataCode(StringUtility.isEmpty(awbShipmentInfo.getIataCode())
                            ? StringUtility.convertToString(shipmentDetails.getConsignee().getOrgData().get(PartiesConstants.AGENT_IATA_CODE))
                            : awbShipmentInfo.getIataCode());
                    awbShipmentInfo.setAgentCASSCode(StringUtility.isEmpty(awbShipmentInfo.getAgentCASSCode())
                            ? StringUtility.convertToString(shipmentDetails.getConsignee().getOrgData().get(PartiesConstants.AGENT_CASS_CODE))
                            : awbShipmentInfo.getAgentCASSCode());
                }
            }
        }

        try {
            TenantModel tenantModel = jsonHelper.convertValue(v1Service.retrieveTenant().getEntity(), TenantModel.class);
            setTenantFieldsInAwbShipmentInfo(awbShipmentInfo, tenantModel);
            if (awbShipmentInfo.getIssuingAgentName() == null || awbShipmentInfo.getIssuingAgentName().isEmpty()) {
                populateIssuingAgent(awbShipmentInfo, tenantModel);
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
            var sortedRoutingList = shipmentDetails.getRoutingsList();
            List<AwbRoutingInfo> res = new ArrayList<>();
            Collections.sort(sortedRoutingList, Comparator.comparing(Routings::getLeg));
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
                res.add(awbRoutingInfo);
            }
            return res;
        }

        else if (shipmentDetails.getCarrierDetails() != null &&
                shipmentDetails.getCarrierDetails().getOriginPort() != null &&
                shipmentDetails.getCarrierDetails().getDestinationPort() != null
        ) {
            var flightDate = shipmentDetails.getCarrierDetails().getEtd();
            AwbRoutingInfo routingInfo = new AwbRoutingInfo();
            routingInfo.setIsShipmentCreated(true);
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

    private AwbCargoInfo generateAwbCargoInfo(ShipmentDetails shipmentDetails, CreateAwbRequest request, List<AwbPackingInfo> awbPackingList) {
        AwbCargoInfo awbCargoInfo = new AwbCargoInfo();
        awbCargoInfo.setNtrQtyGoods(AwbUtility.generateNatureAndQuantGoodsField(shipmentDetails.getGoodsDescription(), shipmentDetails.getVolumetricWeight(), awbPackingList));
        awbCargoInfo.setEntityId(shipmentDetails.getId());
        awbCargoInfo.setEntityType(request.getAwbType());
//        awbCargoInfo.setCarriageValue(shipmentDetails.getGoodsValue() != null ? shipmentDetails.getGoodsValue() : new BigDecimal(0.0)); // field missing
//        awbCargoInfo.setCarriageValue(shipmentDetails.getInsuranceValue() != null ? shipmentDetailsgetInsuranceValue() : new BigDecimal(0.0)); // field missing
        awbCargoInfo.setCustomsValue(new BigDecimal(0.0));
        awbCargoInfo.setCurrency(userContext.getUser().getCompanyCurrency());
        awbCargoInfo.setHandlingInfo(getHandlingInfo(MasterDataType.HAWB_GENERATION));
        awbCargoInfo.setAccountingInfo(awbCargoInfo.getAccountingInfo() == null ? null : awbCargoInfo.getAccountingInfo().toUpperCase());
        awbCargoInfo.setOtherInfo(awbCargoInfo.getOtherInfo() == null ? null : awbCargoInfo.getOtherInfo().toUpperCase());
        awbCargoInfo.setNtrQtyGoods(awbCargoInfo.getNtrQtyGoods() == null ? null : awbCargoInfo.getNtrQtyGoods().toUpperCase());
        awbCargoInfo.setShippingInformation(awbCargoInfo.getShippingInformation() == null ? null : awbCargoInfo.getShippingInformation().toUpperCase());
        awbCargoInfo.setShippingInformationOther(awbCargoInfo.getShippingInformationOther() == null ? null : awbCargoInfo.getShippingInformationOther().toUpperCase());
        if(request.getAwbType().equalsIgnoreCase("DMAWB"))
            awbCargoInfo.setChargeCode(fetchChargeCodes(shipmentDetails.getPaymentTerms()));
        return awbCargoInfo;
    }

    private String getCountryCode(String country) {
        CommonV1ListRequest request = new CommonV1ListRequest();
        List<Object> criteria = new ArrayList<>();
        List<Object> subCriteria1 = Arrays.asList(
                Arrays.asList("ItemType"),
                "=",
                MasterDataType.COUNTRIES.getId()
        );
        List<Object> subCriteria2 = Arrays.asList(
                Arrays.asList("ItemValue"),
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
        awbOtherInfo.setExecutedOn(jsonHelper.convertValue(DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss").format(LocalDateTime.now()), LocalDateTime.class));
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
        if(awbPackingList != null) {
            for (var awbPacking: awbPackingList ) {
                awbPacking.setAwbGoodsDescriptionInfoGuid(awbGoodsDescriptionInfo.getGuid());
            }
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
            // Integer totalPacks = 0;
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
                //awbPacking.setCommodityId(packing.getCommodityId());
                awbPacking.setNetWeight(packing.getNetWeight());
                awbPacking.setNetWeightUnit(packing.getNetWeightUnit());
                awbPacking.setVolumeWeight(packing.getVolumeWeight());
                awbPacking.setVolumeWeightUnit(packing.getVolumeWeightUnit());
                awbPacking.setAwbNumber(shipmentDetails.getHouseBill());
                totalPacks += Integer.parseInt(packing.getPacks());
                awbPackingList.add(awbPacking);
            }

            return awbPackingList;
        }
        return null;
    }

    public ResponseEntity<?> createV1Awb(CommonRequestModel commonRequestModel, boolean checkForSync){
        try{
            AwbRequestV2 request = (AwbRequestV2) commonRequestModel.getData();

            Long entityId = null;
            var awbType = request.getAwbShipmentInfo().getEntityType();
            Optional<ConsolidationDetails> consolidation = consolidationDetailsDao.findByGuid(request.getConsolidationGuid());
            Optional<ShipmentDetails> shipment = shipmentDao.findByGuid(request.getShipmentGuid());

            List<Awb> existingAwb;
            Awb awb = jsonHelper.convertValue(request, Awb.class);

            if(awbType.equals("MAWB") && consolidation.isPresent()){
                entityId = consolidation.get().getId();
                awb.setConsolidationId(entityId);
                existingAwb = awbDao.findByConsolidationId(consolidation.get().getId());
            }
            else if(shipment.isPresent()) {
                entityId = shipment.get().getId();
                awb.setShipmentId(entityId);
                existingAwb = awbDao.findByShipmentId(shipment.get().getId());
            }
            else {
                throw new RunnerException("Shipment/Consolidation not present, Please create that first !");
            }
            if (checkForSync && !Objects.isNull(syncConfig.IS_REVERSE_SYNC_ACTIVE) && !syncConfig.IS_REVERSE_SYNC_ACTIVE) {
                return syncQueueService.saveSyncRequest(SyncingConstants.AWB, StringUtility.convertToString(entityId), request);
            }
            setEntityId(awb, entityId);
            if(existingAwb.isEmpty()){
                // SAVE
            } else {
                // UPDATE
                awb.setId(existingAwb.get(0).getId());
                awb.setGuid(existingAwb.get(0).getGuid());
            }
            awbDao.save(awb);

            if(awbType.equals("MAWB") && consolidation.isPresent()) {
                // Link this MAWB with it's HAWB
                List<ShipmentDetails> shipmentList = consolidation.get().getShipmentsList();
                List<Long> shipmentId = new ArrayList();
                if(shipmentList != null && shipmentList.size() > 0) {
                    for(var i : shipmentList)
                        shipmentId.add(i.getId());
                }
                var listCriteria = constructListCommonRequest("shipmentId", shipmentId, "IN");
                Pair<Specification<Awb>, Pageable> pair = fetchData(listCriteria, Awb.class);
                var hawbListPage = awbDao.findAll(pair.getLeft(), pair.getRight());
                LinkHawbMawb(consolidation.get(), awb, hawbListPage.getContent());
            }

            return ResponseHelper.buildSuccessResponse(jsonHelper.convertValue(awb, AwbResponse.class));

        } catch (Exception e){
            log.error("{}", e);
            return ResponseHelper.buildFailedResponse(e.getMessage());
        }
    }

    private void setEntityId(Awb request, Long entityId){
        if(request.getAwbShipmentInfo() != null)
            request.getAwbShipmentInfo().setEntityId(entityId);
        if(request.getAwbNotifyPartyInfo() != null)
            request.getAwbNotifyPartyInfo().forEach(i -> i.setEntityId(entityId) );
        if(request.getAwbRoutingInfo() != null)
            request.getAwbRoutingInfo().forEach(i -> i.setEntityId(entityId) );
        if(request.getAwbCargoInfo() != null)
            request.getAwbCargoInfo().setEntityId(entityId);
        if(request.getAwbPaymentInfo() != null)
            request.getAwbPaymentInfo().setEntityId(entityId);
        if(request.getAwbOtherChargesInfo() != null)
            request.getAwbOtherChargesInfo().forEach(i -> i.setEntityId(entityId) );
        if(request.getAwbOtherInfo() != null)
            request.getAwbOtherInfo().setEntityId(entityId);
        if(request.getAwbOciInfo() != null)
            request.getAwbOciInfo().forEach(i -> i.setEntityId(entityId) );
        if(request.getAwbGoodsDescriptionInfo() != null)
            request.getAwbGoodsDescriptionInfo().forEach(i -> i.setEntityId(entityId) );
        if(request.getAwbSpecialHandlingCodesMappings() != null)
            request.getAwbSpecialHandlingCodesMappings().forEach(i -> i.setEntityId(entityId) );
    }

    private void updateAwbOtherChargesInfo(List<AwbOtherChargesInfo> otherChargesInfos) {
        if(otherChargesInfos != null && otherChargesInfos.size() > 0) {
            otherChargesInfos.stream().map(i -> {
                if(i.getGuid() == null)
                    i.setGuid(UUID.randomUUID());
                return i;
            }).toList();
        }
    }

    List<Awb> getLinkedAwbFromMawb(Long mawbId) {
        List<MawbHawbLink> mawbHawbLinks = mawbHawbLinkDao.findByMawbId(mawbId);

        // Fetch all the awb records with the mapped hawbId
        ListCommonRequest listCommonRequest = CommonUtils.constructListCommonRequest("id", mawbHawbLinks.stream().map(i -> i.getHawbId()).collect(Collectors.toList()), "IN");
        Pair<Specification<Awb>, Pageable> pair = fetchData(listCommonRequest, Awb.class);
        Page<Awb> page = awbDao.findAll(pair.getLeft(), pair.getRight());

        List<Awb> linkedHawb = new ArrayList<>();
        if(!page.isEmpty())
            linkedHawb = page.getContent();

        return linkedHawb;
    }


    @Async
    private void callV1Sync(Awb entity, SaveStatus saveStatus){
        awbSync.sync(entity, saveStatus);
    }

    public ResponseEntity<?> customAwbRetrieve(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CustomAwbRetrieveRequest request = (CustomAwbRetrieveRequest) commonRequestModel.getData();
            List<String> awbNumber = request.getAwbNumber();
            String issuingAgentName = request.getIssuingAgent();
            List<Awb> awbs = new ArrayList<>();
            if (awbNumber == null && issuingAgentName == null)
                log.error("Request is empty for AWB retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
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
    public ResponseEntity<?> reset(CommonRequestModel commonRequestModel) {
        ResetAwbRequest resetAwbRequest = (ResetAwbRequest) commonRequestModel.getData();
        Optional<Awb> awbOptional = awbDao.findById(resetAwbRequest.getId());

        if (awbOptional.isEmpty()) {
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        Awb awb = awbOptional.get();
        UUID awbGuid = awb.getGuid();
        Long awbId = awb.getId();

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
                            Awb linkAwb = awbDao.findByShipmentId(consoleShipment.getId()).stream().findFirst().get();
                            if (linkAwb == null) {
                                throw new ValidationException("To Generate Mawb, Please create Hawb for all the shipments attached");
                            }
                            awbList.add(linkAwb);
                            if(linkAwb.getAwbPackingInfo() != null) {
                                mawbPackingInfo.addAll(linkAwb.getAwbPackingInfo());
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
                    LinkHawbMawb(consolidationDetails.get(), awb, awbList);
                }
                else awb = generateAwb(createAwbRequest);
                awb.setGuid(awbGuid);
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
                awb.setAwbNotifyPartyInfo(generateAwbNotifyPartyinfo(shipmentDetails.get(), createAwbRequest));
                break;
            }
            case AWB_PACKS_AND_GOODS: {
                if (resetAwbRequest.getAwbType().equals(Constants.MAWB)) {
                    //awb.setAwbPackingInfo(generateMawbPackingInfo(consolidationDetails.get()));
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
            }
            case AWB_OCI_INFO: {
                awb.setAwbOciInfo(null);
            }
        }
        awb.setId(resetAwbRequest.getId());
        awb = awbDao.save(awb);
        try {
            callV1Sync(awb, SaveStatus.RESET);
        } catch (Exception e) {
            log.error("Error performing sync on AWB entity, {}", e);
        }

        return ResponseHelper.buildSuccessResponse(convertEntityToDto(awb));
    }

    @Override
    public ResponseEntity<?> partialAutoUpdateAwb(CommonRequestModel commonRequestModel) {
        String responseMsg;
        CreateAwbRequest request = (CreateAwbRequest) commonRequestModel.getData();
        if (request == null) {
            log.debug("Request is empty for AWB Create for Request Id {}", LoggerHelper.getRequestIdFromMDC());
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

        List<ShipmentSettingsDetails> shipmentSettingsDetailsList = shipmentSettingsDao.getSettingsByTenantIds(List.of(TenantContext.getCurrentTenant()));
        if(shipmentSettingsDetailsList.isEmpty()){
            log.error("Failed to fetch Shipment Settings Details");
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        ShipmentSettingsDetails shipmentSettingsDetails = shipmentSettingsDetailsList.get(0);

        try {
            if(shipmentSettingsDetails.getRestrictAWBEdit()){
                ResetAwbRequest resetAwbRequest = ResetAwbRequest.builder()
                        .id(awb.getId())
                        .shipmentId(request.getShipmentId())
                        .consolidationId(request.getConsolidationId())
                        .awbType(request.getAwbType())
                        .resetType(AwbReset.ALL)
                        .build();
                return this.reset(CommonRequestModel.buildRequest(resetAwbRequest));
            }
            else if(shipmentSettingsDetails.getAutoUpdateShipmentAWB()) {
                updateAwbFromShipment(awb, request, shipmentSettingsDetails);
                awb = awbDao.save(awb);
            }

            log.info("AWB created successfully for Id {} with Request Id {}", awb.getId(), LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
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
        for (var cargo: awb.getAwbPackingInfo()) {
            if(cargo.getGuid() != null) {
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
                //awbPacking.setCommodityId(packing.getCommodityId());
                awbPacking.setNetWeight(packing.getNetWeight());
                awbPacking.setNetWeightUnit(packing.getNetWeightUnit());
                awbPacking.setVolumeWeight(packing.getVolumeWeight());
                awbPacking.setVolumeWeightUnit(packing.getVolumeWeightUnit());
                awbPacking.setAwbNumber(shipmentDetails.getHouseBill());
                totalPacksCount += Integer.parseInt(packing.getPacks());
                awb.getAwbPackingInfo().add(awbPacking);
            }
        }
    }
    private void updateShipmentPackingFieldToHbl(Packing packing, AwbPackingInfo awbPackingInfo, CreateAwbRequest request, HawbLockSettings hawbLockSettings, MawbLockSettings mawbLockSettings) {
        if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getPackingAwbGoodsDescIdLock()) ||
                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getPackingAwbGoodsDescIdLock()))

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
        if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getAirportOfDepartureLock()) ||
                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getAirportOfDepartureLock()))
            awbShipmentInfo.setOriginAirport(shipmentDetails.getCarrierDetails().getOriginPort());
        if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getAirportOfDestinationLock()) ||
                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getAirportOfDestinationLock()))
            awbShipmentInfo.setDestinationAirport(shipmentDetails.getCarrierDetails().getDestinationPort());
        if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getFirstCarrierLock()) ||
                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getFirstCarrierLock()))
            awbShipmentInfo.setFirstCarrier(shipmentDetails.getCarrierDetails().getShippingLine());
        if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getIataCodeLock()) ||
                (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getIataCodeLock()))
            awbShipmentInfo.setIataCode(iataCode);


        for (var orgRow : shipmentDetails.getShipmentAddresses()) {
            if (orgRow.getType() == Constants.FORWARDING_AGENT) {
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
        for(var awbParty: awb.getAwbNotifyPartyInfo()){
            if(awbParty.getIsShipmentCreated() != null && awbParty.getIsShipmentCreated()){
                createNotifyParty = false;
                if(party != null){
                    if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getNotifyOrganizationLock()) ||
                            (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getNotifyOrganizationLock())) {
                        var name = StringUtility.convertToString(party.getOrgData().get(PartiesConstants.FULLNAME));
                        awbParty.setName(name == null ? name : name.toUpperCase());
                        awbParty.setNotifyOrgId(party.getId());
                    }
                    if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getNotifyOrganizationAddressLock()) ||
                            (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getNotifyOrganizationAddressLock())) {
                        awbParty.setAddress(AwbUtility.constructAddress(party.getAddressData()).toUpperCase());
                    }

                } else {
                    deleteParty = awbParty;
                }
            }
        }
        awb.getAwbNotifyPartyInfo().remove(deleteParty);

        AwbNotifyPartyInfo awbParty = AwbNotifyPartyInfo.builder().build();
        if (party != null && createNotifyParty) {
            awbParty.setIsShipmentCreated(true);
            var name = StringUtility.convertToString(party.getOrgData().get(PartiesConstants.FULLNAME));
            awbParty.setName(name == null ? name : name.toUpperCase());
            awbParty.setAddress(AwbUtility.constructAddress(party.getAddressData()).toUpperCase());
            awbParty.setEntityId(shipmentDetails.getId());
            awbParty.setEntityType(request.getAwbType());
            awbParty.setNotifyOrgId(party.getId());
            awb.getAwbNotifyPartyInfo().add(awbParty);
        }
    }
    private void updateShipmemtRoutingInfoToAwb(ShipmentDetails shipmentDetails, CreateAwbRequest request, Awb awb, HawbLockSettings hawbLockSettings, MawbLockSettings mawbLockSettings) {
        boolean createRouting = true;
        AwbRoutingInfo deleteParty = new AwbRoutingInfo();
        for(var awbRoute: awb.getAwbRoutingInfo()){
            if(awbRoute.getIsShipmentCreated() != null && awbRoute.getIsShipmentCreated()){
                createRouting = false;
                if (shipmentDetails.getCarrierDetails() != null &&
                        shipmentDetails.getCarrierDetails().getOriginPort() != null &&
                        shipmentDetails.getCarrierDetails().getDestinationPort() != null) {
                    if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getOriginPortLock()) ||
                            (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getOriginPortLock())) {
                        awbRoute.setOriginPortName(shipmentDetails.getCarrierDetails().getOriginPort());
                    }
                    if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getDestinationPortLock()) ||
                            (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getDestinationPortLock())) {
                        awbRoute.setDestinationPortName(shipmentDetails.getCarrierDetails().getDestinationPort());
                    }
                    if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getByCarrierLock()) ||
                            (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getByCarrierLock()))
                        awbRoute.setByCarrier(shipmentDetails.getCarrierDetails().getShippingLine());
                    if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getFlightNumberLock()) ||
                            (request.getAwbType().equals(Constants.DMAWB) && !mawbLockSettings.getFlightNumberLock()))
                        awbRoute.setFlightNumber(shipmentDetails.getCarrierDetails().getFlightNumber());
                    if((request.getAwbType().equals(Constants.HAWB) && !hawbLockSettings.getFlightDateLock()) ||
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
            awb.getAwbRoutingInfo().add(routingInfo);
        }
    }
    private void updateAwbGoodsDescriptionInfoFromShipment(ShipmentDetails shipmentDetails, CreateAwbRequest request, Awb awb, HawbLockSettings hawbLockSettings, MawbLockSettings mawbLockSettings) {
        List<AwbGoodsDescriptionInfo> awbGoodsDescriptionInfoList = new ArrayList<>();
        if(!awb.getAwbGoodsDescriptionInfo().isEmpty())
            awbGoodsDescriptionInfoList = awb.getAwbGoodsDescriptionInfo().stream().filter(good -> good.getIsShipmentCreated() != null && good.getIsShipmentCreated()).toList();
        if(awb.getAwbGoodsDescriptionInfo().isEmpty() || awbGoodsDescriptionInfoList.isEmpty()){
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
                        awbGoodsDescriptionInfo.getAwbPackingInfo().add(awbPacking);
                    }
                }
            }
            awbGoodsDescriptionInfo.setPiecesNo(totalPacksCount);
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
                    if(awbPacking.getAwbGoodsDescriptionInfoGuid().equals(awbGoodsDescriptionInfo.getGuid()))
                        totalPacksCount += Integer.parseInt(awbPacking.getPacks());
                    if(awbPacking.getGuid() != null && awbPacking.getAwbGoodsDescriptionInfoGuid() == null) {
                        awbPacking.setAwbGoodsDescriptionInfoGuid(awbGoodsDescriptionInfo.getGuid());
                        totalPacksCount += Integer.parseInt(awbPacking.getPacks());
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
            awbCargoInfo.setNtrQtyGoods(AwbUtility.generateNatureAndQuantGoodsField(shipmentDetails.getGoodsDescription(), shipmentDetails.getVolumetricWeight(), awb.getAwbPackingInfo()));
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
            awbOtherInfo.setExecutedOn(jsonHelper.convertValue(DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss").format(LocalDateTime.now()), LocalDateTime.class));
    }

    @Override
    public ResponseEntity<?> partialAutoUpdateMawb(CommonRequestModel commonRequestModel) {
        String responseMsg;
        CreateAwbRequest request = (CreateAwbRequest) commonRequestModel.getData();
        if (request == null) {
            log.debug("Request is empty for MAWB Create for Request Id {}", LoggerHelper.getRequestIdFromMDC());
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

        List<ShipmentSettingsDetails> shipmentSettingsDetailsList = shipmentSettingsDao.getSettingsByTenantIds(List.of(TenantContext.getCurrentTenant()));
        if(shipmentSettingsDetailsList.isEmpty()){
            log.error("Failed to fetch Shipment Settings Details");
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        ShipmentSettingsDetails shipmentSettingsDetails = shipmentSettingsDetailsList.get(0);
        try {
            // fetch consolidation info
            ConsolidationDetails consolidationDetails = consolidationDetailsDao.findById(request.getConsolidationId()).get();
            if(shipmentSettingsDetails.getRestrictAWBEdit()){
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
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(awb));
    }

    private void updateLinkHawbMawb(ConsolidationDetails consolidationDetails, Long mawbId) {
        List<MawbHawbLink> mawbHawbLinks = mawbHawbLinkDao.findByMawbId(mawbId);
        Set<Long> linkedHawbIds = new HashSet<>();
        linkedHawbIds = mawbHawbLinks.stream().map(link -> link.getHawbId()).collect(Collectors.toSet());
        for (var consoleShipment : consolidationDetails.getShipmentsList()) {
            if (consoleShipment.getId() != null) {
                Awb awb = awbDao.findByShipmentId(consoleShipment.getId()).stream().findFirst().get();

                if(awb != null && !linkedHawbIds.contains(awb.getId())) {
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
        awb.setAwbPackingInfo(updateMawbPackingInfoFromShipment(consolidationDetails));
        updateMawbShipmentInfoFromShipment(consolidationDetails, request, awb, mawbLockSettings);
        generateMawbNotifyPartyinfo(consolidationDetails, request, awb, mawbLockSettings);
        updateMawbRoutingInfoFromShipment(consolidationDetails, request, awb, mawbLockSettings);
        updateMawbCargoInfoFromShipment(consolidationDetails, request, awb, mawbLockSettings);
        generateMawbOtherInfo(consolidationDetails, request, awb, mawbLockSettings);
    }
    private List<AwbPackingInfo> updateMawbPackingInfoFromShipment(ConsolidationDetails consolidationDetails) {
        List<AwbPackingInfo> awbPackingList = new ArrayList<>();
        List<AwbGoodsDescriptionInfo> awbGoodsDescList = new ArrayList<>();
        List<Long> attachedHawbIds = new ArrayList<>();
        List<AwbPackingInfo> hawbPacksLinkedToMawb = new ArrayList<>();

        if (consolidationDetails.getShipmentsList().size() > 0) {
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
        awbShipmentInfo.setOriginAirport(consolidationDetails.getCarrierDetails() != null ? consolidationDetails.getCarrierDetails().getOriginPort() : null);
        awbShipmentInfo.setDestinationAirport(consolidationDetails.getCarrierDetails() != null ? consolidationDetails.getCarrierDetails().getDestinationPort() : null);
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
        }
        awb.getAwbNotifyPartyInfo().removeAll(deleteAwbPartyList);

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
                awb.getAwbNotifyPartyInfo().add(notifyPartyInfo);
            }
        }

    }

    private void updateMawbRoutingInfoFromShipment(ConsolidationDetails consolidationDetails, CreateAwbRequest request, Awb awb, MawbLockSettings mawbLockSettings) {
        boolean createRouting = true;
        AwbRoutingInfo deleteParty = new AwbRoutingInfo();
        for(var awbRoute: awb.getAwbRoutingInfo()){
            if(awbRoute.getIsShipmentCreated() != null && awbRoute.getIsShipmentCreated()){
                createRouting = false;
                if (consolidationDetails.getCarrierDetails() != null &&
                        consolidationDetails.getCarrierDetails().getOriginPort() != null &&
                        consolidationDetails.getCarrierDetails().getDestinationPort() != null) {
                    if(!mawbLockSettings.getOriginPortLock()) {
                        awbRoute.setOriginPortName(consolidationDetails.getCarrierDetails().getOriginPort());
                    }
                    if(!mawbLockSettings.getDestinationPortLock()) {
                        awbRoute.setDestinationPortName(consolidationDetails.getCarrierDetails().getDestinationPort());
                    }
                    if(!mawbLockSettings.getByCarrierLock())
                        awbRoute.setByCarrier(consolidationDetails.getCarrierDetails().getShippingLine());
                    if(!mawbLockSettings.getFlightNumberLock())
                        awbRoute.setFlightNumber(consolidationDetails.getCarrierDetails().getFlightNumber());
                } else {
                    deleteParty = awbRoute;
                }
            }
        }
        awb.getAwbRoutingInfo().remove(deleteParty);

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
            awbCargoInfo.setNtrQtyGoods(AwbUtility.generateNatureAndQuantFieldsForConsolMawb(concatenatedGoodsDesc, totalVolumetricWeightOfAwbPacks, awb.getAwbPackingInfo()));
            awbCargoInfo.setNtrQtyGoods(awbCargoInfo.getNtrQtyGoods() == null ? null : awbCargoInfo.getNtrQtyGoods().toUpperCase());
        }

//        awbCargoInfo.setCarriageValue(shipmentDetails.getGoodsValue() != null ? shipmentDetails.getGoodsValue() : new BigDecimal(0.0)); // field missing
//        awbCargoInfo.setCarriageValue(shipmentDetails.getInsuranceValue() != null ? shipmentDetailsgetInsuranceValue() : new BigDecimal(0.0)); // field missing
        if(!mawbLockSettings.getCustomsValueLock())
            awbCargoInfo.setCustomsValue(new BigDecimal(0.0));
        if(!mawbLockSettings.getCurrencyLock())
            awbCargoInfo.setCurrency(userContext.getUser().getCompanyCurrency());
        awbCargoInfo.setHandlingInfo(getHandlingInfo(MasterDataType.MAWB_GENERATION));
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
            awbOtherInfo.setExecutedOn(jsonHelper.convertValue(DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss").format(LocalDateTime.now()), LocalDateTime.class));

    }

    private void setTenantFieldsInAwbShipmentInfo(AwbShipmentInfo awbShipmentInfo, TenantModel tenantModel) {
        iataCode = tenantModel.AgentIATACode;
        awbShipmentInfo.setIataCode(iataCode);
        awbShipmentInfo.setAgentCASSCode(tenantModel.AgentCASSCode);
    }

    private String getHandlingInfo(MasterDataType masterDataType) {
        String res = null;
        List<Integer> itemTypeList = new ArrayList<>();
        itemTypeList.add(masterDataType.getId());
        List<Object> masterDataCriteria = Arrays.asList(
                Arrays.asList("ItemType"),
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
        return res;
    }

    @Override
    public ResponseEntity<?> getAllMasterData(CommonRequestModel commonRequestModel, boolean isShipment) {
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

    private CompletableFuture<ResponseEntity<?>> addAllMasterDataInSingleCall (Awb awb, AwbResponse awbResponse, Map<String, Object> masterDataResponse) {

        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        AtomicInteger count = new AtomicInteger();
        List<MasterListRequest> listRequests = new ArrayList<>(masterDataUtils.createInBulkMasterListRequest(awbResponse, Awb.class, fieldNameKeyMap, Awb.class.getSimpleName() ));
        // Populate all the master data in inner objects
        if (!Objects.isNull(awbResponse.getAwbShipmentInfo()))
            listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(awbResponse.getAwbShipmentInfo(), AwbShipmentInfo.class, fieldNameKeyMap, AwbShipmentInfo.class.getSimpleName() ));
        if(!Objects.isNull(awbResponse.getAwbRoutingInfo()))
            awbResponse.getAwbRoutingInfo().forEach(r -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(r, AwbRoutingInfo.class, fieldNameKeyMap, AwbRoutingInfo.class.getSimpleName() + count.incrementAndGet())));
        if(!Objects.isNull(awbResponse.getAwbPackingInfo()))
            awbResponse.getAwbPackingInfo().forEach(r -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(r, AwbPackingInfo.class, fieldNameKeyMap, AwbPackingInfo.class.getSimpleName() + count.incrementAndGet() )));
        if(!Objects.isNull(awbResponse.getAwbGoodsDescriptionInfo()))
            awbResponse.getAwbGoodsDescriptionInfo().forEach(r -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(r, AwbGoodsDescriptionInfo.class, fieldNameKeyMap, AwbGoodsDescriptionInfo.class.getSimpleName() + count.incrementAndGet())));

        MasterListRequestV2 masterListRequestV2 = new MasterListRequestV2();
        masterListRequestV2.setMasterListRequests(listRequests);
        masterListRequestV2.setIncludeCols(Arrays.asList("ItemType", "ItemValue", "ItemDescription", "ValuenDesc", "Cascade"));

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
    }

    private CompletableFuture<ResponseEntity<?>> addAllUnlocationDataInSingleCall (Awb awb, AwbResponse awbResponse, Map<String, Object> masterDataResponse) {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        AtomicInteger count = new AtomicInteger();
        List<String> locationCodes = new ArrayList<>();
        // Populate all the unlocation data in inner objects
        if (!Objects.isNull(awbResponse.getAwbShipmentInfo()))
            locationCodes.addAll((masterDataUtils.createInBulkUnLocationsRequest(awbResponse.getAwbShipmentInfo(), AwbShipmentInfo.class, fieldNameKeyMap, AwbShipmentInfo.class.getSimpleName() )));
        if (!Objects.isNull(awbResponse.getAwbOtherInfo()))
            locationCodes.addAll((masterDataUtils.createInBulkUnLocationsRequest(awbResponse.getAwbOtherInfo(), AwbOtherInfo.class, fieldNameKeyMap, AwbShipmentInfo.class.getSimpleName() )));
        if(!Objects.isNull(awbResponse.getAwbRoutingInfo()))
            awbResponse.getAwbRoutingInfo().forEach(r -> locationCodes.addAll(masterDataUtils.createInBulkUnLocationsRequest(r, AwbRoutingInfo.class, fieldNameKeyMap, AwbRoutingInfo.class.getSimpleName() + (count.incrementAndGet()))));
        if(!Objects.isNull(awbResponse.getAwbPackingInfo()))
            awbResponse.getAwbPackingInfo().forEach(r -> locationCodes.addAll(masterDataUtils.createInBulkUnLocationsRequest(r, AwbPackingInfo.class, fieldNameKeyMap, AwbPackingInfo.class.getSimpleName() + (count.incrementAndGet()))));
        if(!Objects.isNull(awbResponse.getAwbGoodsDescriptionInfo()))
            awbResponse.getAwbGoodsDescriptionInfo().forEach(r -> locationCodes.addAll(masterDataUtils.createInBulkUnLocationsRequest(r, AwbGoodsDescriptionInfo.class, fieldNameKeyMap, AwbGoodsDescriptionInfo.class.getSimpleName() + (count.incrementAndGet()))));

        Map<String, EntityTransferUnLocations> keyMasterDataMap = masterDataUtils.fetchInBulkUnlocations(locationCodes, EntityTransferConstants.LOCATION_SERVICE_GUID);
        masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.UNLOCATIONS_AWB);

        if(masterDataResponse == null) {
            if (!Objects.isNull(awbResponse.getAwbShipmentInfo()))
                awbResponse.getAwbShipmentInfo().setUnlocationData(masterDataUtils.setMasterData(fieldNameKeyMap.get(AwbShipmentInfo.class.getSimpleName()), CacheConstants.UNLOCATIONS_AWB));
        }
        else {
            masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.UNLOCATIONS_AWB, masterDataResponse);
        }

        return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(keyMasterDataMap));
    }
    private CompletableFuture<ResponseEntity<?>> addAllCommodityTypesInSingleCallPacksList(Awb awb, AwbResponse awbResponse, Map<String, Object> masterDataResponse) {
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
    }

    @Override
    public ResponseEntity<?> generateAwbPaymentInfo(CommonRequestModel commonRequestModel) {

        GenerateAwbPaymentInfoRequest req = (GenerateAwbPaymentInfoRequest) commonRequestModel.getData();

        double totalAmount = 0.00;
        if(req.getAwbGoodsDescriptionInfo() != null) {
            for(var goods : req.getAwbGoodsDescriptionInfo()) {
                totalAmount += goods.getTotalAmount() != null ? goods.getTotalAmount().doubleValue() : 0;
            }
        }

        double agentOtherCharges = calculateOtherCharges(req, ChargesDue.AGENT);
        double carrierOtherCharges = calculateOtherCharges(req, ChargesDue.CARRIER);

        AwbPaymentInfo paymentInfo;
        if(req.getAwbPaymentInfo() != null ){
            paymentInfo = req.getAwbPaymentInfo();
            paymentInfo.setWeightCharges(new BigDecimal(totalAmount));
            paymentInfo.setDueAgentCharges(agentOtherCharges != 0 ? new BigDecimal(agentOtherCharges) : null);
            paymentInfo.setDueCarrierCharges(carrierOtherCharges != 0 ? new BigDecimal(carrierOtherCharges) : null);
        } else {
            paymentInfo = AwbPaymentInfo.builder()
                    .weightCharges(new BigDecimal(totalAmount))
                    .dueAgentCharges(agentOtherCharges != 0 ? new BigDecimal(agentOtherCharges) : null)
                    .dueCarrierCharges(carrierOtherCharges != 0 ? new BigDecimal(carrierOtherCharges) : null)
                    .build();
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

            if(req.getChargeDetails().getIdentifier1().equals(Constants.TRUE)) {
                // Prepaid WeighCharges
                prepaidWeightCharge = totalAmount;
                prepaidValuationCharge = getDoubleValue(req.getAwbPaymentInfo().getValuationCharge());
                prepaidTax = getDoubleValue(req.getAwbPaymentInfo().getTax());
            } else {
                prepaidWeightCharge = 0.00;
                prepaidValuationCharge = 0.00;
                prepaidTax = 0.00;
            }

            if(req.getChargeDetails().getIdentifier2().equals(Constants.TRUE)) {
                // CollectWeightCharges
                collectWeightCharge = totalAmount;
                collectValuationCharge = getDoubleValue(req.getAwbPaymentInfo().getValuationCharge());
                collectTax = getDoubleValue(req.getAwbPaymentInfo().getTax());
            } else {
                collectWeightCharge = 0.00;
                collectValuationCharge = 0.00;
                collectTax = 0.00;
            }

            if(req.getChargeDetails().getIdentifier3().equals(Constants.TRUE)) {
                // PrepaidDueAgentCharges
                // PrepaidDueCarrierCharges
                prepaidDueAgentCharges = agentOtherCharges;
                prepaidDueCarrierCharges = carrierOtherCharges;
            } else{
                prepaidDueAgentCharges = 0.00;
                prepaidDueCarrierCharges = 0.00;
            }

            if(req.getChargeDetails().getIdentifier4().equals(Constants.TRUE)) {
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

        /*
            1. Calculate good description based on packs
            2. Calculate other infos based on good description
         */
        updateGoodsDescriptionInfoFromPacks(req);
        updateOtherChargesFromGoodsDescription(req);

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

    public ResponseEntity<?> retrieveByAwbByMawb(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for AWB retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.error("Request Id is null for MAWB retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            List<Awb> awb = getLinkedAwbFromMawb(id);
            if (awb == null) {
                log.debug("AWB is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("AWB fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());

            if(request.getIncludeColumns()==null||request.getIncludeColumns().size()==0)
                return ResponseHelper.buildSuccessResponse(jsonHelper.convertValueToList(awb, AwbResponse.class));
            else {
                List<Object> data = new ArrayList<>();
                for(Awb awb1 : awb) {
                    data.add(PartialFetchUtils.fetchPartialListData(jsonHelper.convertValue(awb, AwbResponse.class), request.getIncludeColumns()));
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

    private BigDecimal updateGoodsDescriptionInfoFromPacks(GenerateAwbPaymentInfoRequest request) {
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
                if (!Objects.isNull(request.getIsFromShipment()) && request.getIsFromShipment())
                    guidBasedAwbPackingList = packsInfo.stream().filter(c -> !Objects.isNull(c.getAwbGoodsDescriptionInfoGuid()))
                            .collect(Collectors.groupingBy(AwbPackingInfo::getAwbGoodsDescriptionInfoGuid));
                else
                    guidBasedAwbPackingList.put(goodsDescriptionInfos.get(0).getGuid(), packsInfo);
            }

            for (int i = 0; i < goodsDescriptionInfos.size(); i++) {
                var allPacks = guidBasedAwbPackingList.get(goodsDescriptionInfos.get(i).getGuid());
                Pair<BigDecimal, AwbGoodsDescriptionInfo> pair = calculateGoodsDescription(goodsDescriptionInfos.get(i), allPacks, tenantSettings, new HashMap<>());
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
    public ResponseEntity<?> generateUpdatedNatureAndQuantGoodsField(CommonRequestModel commonRequestModel) {
        GenerateAwbPaymentInfoRequest request = (GenerateAwbPaymentInfoRequest) commonRequestModel.getData();
        String natureAndQuantGoodsValue = request.getAwbCargoInfo() == null || request.getAwbCargoInfo().getNtrQtyGoods() == null ? null : request.getAwbCargoInfo().getNtrQtyGoods();
        String packsDescriptionValue = "";
        String dimensionText = Constants.DEFAULT_DIMN_TEXT;
        Set<String> uniqueDimension = new HashSet<>();
        String newLine = "\r\n";

        if (StringUtility.isNotEmpty(natureAndQuantGoodsValue)) {
            natureAndQuantGoodsValue += newLine;
        }
        if (request.getAwbPackingInfo() != null) {
            int counter = 0;
            for (AwbPackingInfo packings : request.getAwbPackingInfo()) {
                String pcs = " ";
                String len = " ";
                String width = " ";
                String height = " ";
                String equals = Constants.EQUALS;
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
                counter++;

                packsDescriptionValue += pcs + len + width + height + ",";
//                if (counter == listOfPackingRows.length) {
//                    packsDescriptionValue = packsDescriptionValue.slice(0, -1);
//                }

                if (counter % 2 == 0) {
                    packsDescriptionValue += newLine;
                }
            }

            if (uniqueDimension.size() == 1) {
                String dimentionUnit = new ArrayList<>(uniqueDimension).get(0);
                if (dimentionUnit == Constants.CM) {
                    dimentionUnit = Constants.CMS;
                } else if (dimentionUnit == Constants.IN) {
                    dimentionUnit = Constants.INCH;
                } else if (dimentionUnit == Constants.M) {
                    dimentionUnit = Constants.MTR;
                } else if (dimentionUnit == Constants.FT) {
                    dimentionUnit = Constants.FEET;
                } else {
                    dimentionUnit = "";
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

            packsDescriptionValue += "Total Volumetric Weight " + totalVWt.toString() + " " + "";

        }

        return ResponseHelper.buildSuccessResponse(packsDescriptionValue);
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
                defaultAwbShipmentInfo = jsonHelper.convertValue(generateMawbShipmentInfo(consol, createAwbRequest), AwbShipmentInfoResponse.class);
                defaultRoutingInfo = jsonHelper.convertValueToList(generateMawbRoutingInfo(consol, createAwbRequest), AwbRoutingInfoResponse.class);
                defaultNotifyPartyInfo = generateMawbNotifyPartyinfo(consol, createAwbRequest);
            } else {
                ShipmentDetails shipment = shipmentDao.findById(awb.getShipmentId()).get();
                defaultAwbShipmentInfo = jsonHelper.convertValue(generateAwbShipmentInfo(shipment, createAwbRequest), AwbShipmentInfoResponse.class);
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

    private void setAwbShipmentInfoUnLocationData(AwbShipmentInfo awbShipmentInfo, CarrierDetails carrierDetails) {

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
        var locMap = locationDataList.stream().collect(Collectors.toMap(EntityTransferUnLocations::getLocationsReferenceGUID, EntityTransferUnLocations::getName));

        awbShipmentInfo.setOriginAirport(locMap.get(carrierDetails.getOriginPort()));
        awbShipmentInfo.setDestinationAirport(locMap.get(carrierDetails.getDestinationPort()));
    }

    private void getAwbOtherInfoMasterData(AwbOtherInfo awbOtherInfo, String awbType) {
        MasterDataType masterDataType;
        if(awbType.equalsIgnoreCase("HAWB")) {
            masterDataType = MasterDataType.HAWB_CARRIER_AGENT;
        } else {
            masterDataType = MasterDataType.MAWB_CARRIER_AGENT;
        }
        List<Object> criteria = Arrays.asList(
                List.of("ItemType"),
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
                "in",
                List.of(name)
        );
        CommonV1ListRequest commonV1ListRequest = CommonV1ListRequest.builder().skip(0).take(0).criteriaRequests(criteria).build();
        V1DataResponse v1DataResponse = v1Service.fetchUnlocation(commonV1ListRequest);

        List<EntityTransferUnLocations> locationDataList = jsonHelper.convertValueToList(v1DataResponse.entities, EntityTransferUnLocations.class);
        var locMap = locationDataList.stream().collect(Collectors.toMap(EntityTransferUnLocations::getNameWoDiacritics, EntityTransferUnLocations::getLocationsReferenceGUID,
                (locationguid1, locationguid2) -> {
            return locationguid1;
        }));
        return locMap.get(name);
    }

    private void populateIssuingAgent(AwbShipmentInfo awbShipmentInfo, TenantModel tenantModel) {
        if(tenantModel.DefaultOrgId != null){
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
                    awbShipmentInfo.setIssuingAgentName(orgList.get(0).getFullName());
                    awbShipmentInfo.setIataCode(awbShipmentInfo.getIataCode() == null ? orgList.get(0).getAgentIATACode() : awbShipmentInfo.getIataCode());
                    awbShipmentInfo.setAgentCASSCode(awbShipmentInfo.getAgentCASSCode() == null ?
                            orgList.get(0).getAgentCASSCode() : awbShipmentInfo.getAgentCASSCode());
                    awbShipmentInfo.setIssuingAgentAddress(getFormattedAddress(orgList.get(0)));
                }

            } catch (Exception e) {
                throw new RunnerException(String.format("Failed to fetch organization data for default org : %s", tenantModel.DefaultOrgId));
            }
        }
    }

    private String validateMawb(Awb awb) {
        List<String> errors = new ArrayList<>();
        boolean allHawbsGenerated = true;
        var id = awb.getConsolidationId();
        if(id == null) {
            throw new RunnerException("ID can't be null, please provide a valid input !");
        }

        List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByConsolidationId(id);
        List<Long> shipmentIdList = consoleShipmentMappings.stream().map(ConsoleShipmentMapping::getShipmentId).collect(Collectors.toList());
        // Check whether HAWB is generated for all the linked shipments
        for(var shipmentId : shipmentIdList) {
            List<Awb> response = awbDao.findByShipmentId(shipmentId);
            if (Objects.isNull(response) || response.size() == 0) {
              allHawbsGenerated = false;
              break;
            }
        }

        if(!allHawbsGenerated)
            errors.add("Additional Shipments have been attached, please reset data as required.");

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

}