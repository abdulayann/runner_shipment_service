package com.dpw.runner.shipment.services.service.impl;


import com.dpw.runner.shipment.services.Kafka.Dto.KafkaResponse;
import com.dpw.runner.shipment.services.Kafka.Producer.KafkaProducer;
import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.adapters.interfaces.IOrderManagementAdapter;
import com.dpw.runner.shipment.services.adapters.interfaces.ITrackingServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.*;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerPartialListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.*;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.VolumeWeightChargeable;
import com.dpw.runner.shipment.services.dto.TrackingService.UniversalTrackingPayload;
import com.dpw.runner.shipment.services.dto.patchRequest.CarrierPatchRequest;
import com.dpw.runner.shipment.services.dto.patchRequest.ShipmentPatchRequest;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.dto.v1.request.*;
import com.dpw.runner.shipment.services.dto.v1.response.*;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.*;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCommodityType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferContainerType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferMasterLists;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferUnLocations;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.mapper.CarrierDetailsMapper;
import com.dpw.runner.shipment.services.mapper.ShipmentDetailsMapper;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequest;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequestV2;
import com.dpw.runner.shipment.services.service.interfaces.*;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.dpw.runner.shipment.services.service_bus.AzureServiceBusTopic;
import com.dpw.runner.shipment.services.service_bus.ISBProperties;
import com.dpw.runner.shipment.services.service_bus.ISBUtils;
import com.dpw.runner.shipment.services.syncing.interfaces.IConsolidationSync;
import com.dpw.runner.shipment.services.syncing.interfaces.IHblSync;
import com.dpw.runner.shipment.services.syncing.interfaces.IPackingsSync;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSync;
import com.dpw.runner.shipment.services.utils.*;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.util.CellRangeAddress;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.support.TransactionTemplate;
import org.springframework.util.StringUtils;

import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.OutputStream;
import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.*;
import static com.dpw.runner.shipment.services.utils.StringUtility.isNotEmpty;
import static com.dpw.runner.shipment.services.utils.UnitConversionUtility.convertUnit;

@SuppressWarnings("ALL")
@Service
@Slf4j
public class ShipmentService implements IShipmentService {

    ExecutorService executorService = Executors.newFixedThreadPool(10);
    @Autowired
    private ModelMapper modelMapper;

    @Autowired
    private ObjectMapper objectMapper;
    @Autowired
    private ShipmentDetailsMapper shipmentDetailsMapper;
    @Autowired
    private CarrierDetailsMapper carrierDetailsMapper;

    private final CSVParsingUtil<ShipmentDetails> parser = new CSVParsingUtil<>(ShipmentDetails.class);

    @Autowired
    private IShipmentDao shipmentDao;
    @Autowired
    private ICarrierDao carrierDao;
    @Autowired
    private IPartiesDao partiesDao;

    @Autowired
    private IAdditionalDetailDao additionalDetailDao;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private IPackingDao packingDao;

    @Autowired
    private IPackingsSync packingsSync;

    @Autowired
    private IPackingService packingService;

    @Autowired
    private IContainerService containerService;

    @Autowired
    private IBookingCarriageDao bookingCarriageDao;

    @Autowired
    private IELDetailsDao elDetailsDao;

    @Autowired
    private IEventDao eventDao;

    @Autowired
    private IFileRepoDao fileRepoDao;

    @Autowired
    private IJobDao jobDao;

    @Autowired
    private INotesDao notesDao;

    @Autowired
    private IPickupDeliveryDetailsDao pickupDeliveryDetailsDao;

    @Autowired
    private IReferenceNumbersDao referenceNumbersDao;

    @Autowired
    private IRoutingsDao routingsDao;

    @Autowired
    private IServiceDetailsDao serviceDetailsDao;

    @Autowired
    private IContainerDao containerDao;

    @Autowired
    private TransactionTemplate transactionTemplate;

    @Autowired
    private IShipmentSettingsDao shipmentSettingsDao;

    @Autowired
    UserContext userContext;

    @Autowired
    private IV1Service v1Service;

    @Autowired
    private IConsolidationDetailsDao consolidationDetailsDao;

    @Autowired
    private IConsolidationService consolidationService;

    @Autowired
    private ISBUtils sbUtils;

    @Autowired
    private ISBProperties isbProperties;

    @Autowired
    private AzureServiceBusTopic azureServiceBusTopic;
    @Autowired
    private MasterDataUtils masterDataUtils;

    @Autowired
    private MasterDataKeyUtils masterDataKeyUtils;

    @Autowired
    private IAuditLogService auditLogService;
    @Autowired
    IShipmentSync shipmentSync;
    @Autowired
    IHblService hblService;
    @Autowired
    IConsoleShipmentMappingDao consoleShipmentMappingDao;
    @Autowired
    private IConsolidationSync consolidationSync;

    @Autowired
    private CommonUtils commonUtils;
    @Autowired
    private ISequenceIncrementorDao sequenceIncrementorDao;

    @Autowired
    private IOrderManagementAdapter orderManagementAdapter;

    @Value("${shipmentsKafka.queue}")
    private String senderQueue;

    @Autowired
    private KafkaProducer producer;

    @Autowired
    private ITrackingServiceAdapter trackingServiceAdapter;

    @Autowired
    private IShipmentsContainersMappingDao shipmentsContainersMappingDao;

    @Autowired
    private GetNextNumberHelper getNextNumberHelper;

    @Autowired
    private IHblSync hblSync;

    @Autowired
    private V1ServiceUtil v1ServiceUtil;

    private ShipmentDetails currentShipment;

    @Autowired
    private ProductIdentifierUtility productEngine;

    private List<String> TRANSPORT_MODES = Arrays.asList("SEA", "ROAD", "RAIL", "AIR");
    private List<String> SHIPMENT_TYPE = Arrays.asList("FCL", "LCL");
    private List<String> WEIGHT_UNIT = Arrays.asList("KGS", "G", "DT");
    private List<String> VOLUME_UNIT = Arrays.asList("M3", "L3", "CC");
    private List<String> SHIPPING_LINE = Arrays.asList("DPWC", "MARUSK", "APLU");
    private List<String> LOCATIONS = Arrays.asList("Jabel Ali", "Nava Shiva", "Shanghai", "Vancouver", "Seattle");
    private List<String> PARTY_TYPE = Arrays.asList("CLIENT", "CONSIGNER", "CONSIGNEE");
    private List<String> DIRECTIONS = Arrays.asList("IMP", "EXP");
    private List<String> SOURCE = Arrays.asList("API", "Runner", "Logistics");

    private Map<String, Object> ADDRESS = Map.ofEntries(
            Map.entry("AddressShortCode", "Default")
    );
    private Map<String, Object> ORG = Map.ofEntries(
            Map.entry("TenantName", "DP WORLD LOGISTICS CANADA INC")
    );
    private Map<String, RunnerEntityMapping> tableNames = Map.ofEntries(
            Map.entry("clientOrgCode", RunnerEntityMapping.builder().tableName("client").dataType(String.class).fieldName("orgCode").isContainsText(true).build()),
            Map.entry("consignerOrgCode", RunnerEntityMapping.builder().tableName("consigner").dataType(String.class).fieldName("orgCode").isContainsText(true).build()),
            Map.entry("consigneeOrgCode", RunnerEntityMapping.builder().tableName("consignee").dataType(String.class).fieldName("orgCode").isContainsText(true).build()),
            Map.entry("clientAddressCode", RunnerEntityMapping.builder().tableName("client").dataType(Integer.class).fieldName("addressCode").isContainsText(true).build()),
            Map.entry("consignerAddressCode", RunnerEntityMapping.builder().tableName("consigner").dataType(String.class).fieldName("addressCode").isContainsText(true).build()),
            Map.entry("consigneeAddressCode", RunnerEntityMapping.builder().tableName("consignee").dataType(String.class).fieldName("addressCode").isContainsText(true).build()),
            Map.entry("houseBill", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("houseBill").isContainsText(true).build()),
            Map.entry("houseBillType", RunnerEntityMapping.builder().tableName("additionalDetails").dataType(String.class).fieldName("houseBillType").isContainsText(true).build()),
            Map.entry("transportMode", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("transportMode").isContainsText(true).build()),
            Map.entry("releaseType", RunnerEntityMapping.builder().tableName("additionalDetails").dataType(String.class).fieldName("releaseType").isContainsText(true).build()),
            Map.entry("deliveryMode", RunnerEntityMapping.builder().tableName("additionalDetails").dataType(String.class).fieldName("deliveryMode").isContainsText(true).build()),
            Map.entry("direction", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("direction").isContainsText(true).build()),
            Map.entry("shipmentType", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("shipmentType").isContainsText(true).build()),
            Map.entry("status", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(Integer.class).fieldName("status").build()),
            Map.entry("guid", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(UUID.class).fieldName("guid").build()),
            Map.entry("source", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("source").isContainsText(true).build()),
            Map.entry("jobType", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("jobType").isContainsText(true).build()),
            Map.entry("createdBy", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("createdBy").isContainsText(true).build()),
            Map.entry("serviceType", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("serviceType").isContainsText(true).build()),
            Map.entry("masterBill", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("masterBill").isContainsText(true).build()),
            Map.entry("bookingReference", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("bookingReference").isContainsText(true).build()),
            Map.entry("consolRef", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("consolRef").isContainsText(true).build()),
            Map.entry("salesAgent", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(Long.class).fieldName("salesAgent").build()),
            Map.entry("paymentTerms", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("paymentTerms").isContainsText(true).build()),
            Map.entry("incoterms", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("incoterms").isContainsText(true).build()),
            Map.entry("shipmentId", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("shipmentId").isContainsText(true).build()),
            Map.entry("isDomestic", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(Boolean.class).fieldName("isDomestic").build()),
            Map.entry("assignedTo", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(Integer.class).fieldName("assignedTo").build()),
            Map.entry("additionalTerms", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("additionalTerms").isContainsText(true).build()),
            Map.entry("goodsDescription", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("goodsDescription").isContainsText(true).build()),
            Map.entry("createdAt", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(LocalDateTime.class).fieldName("createdAt").build()),
            Map.entry("updatedAt", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(LocalDateTime.class).fieldName("updatedAt").build()),
            Map.entry("deliveryEstimated", RunnerEntityMapping.builder().tableName("deliveryDetails").dataType(LocalDateTime.class).fieldName("estimatedPickupOrDelivery").build()),
            Map.entry("deliveryActual", RunnerEntityMapping.builder().tableName("deliveryDetails").dataType(LocalDateTime.class).fieldName("actualPickupOrDelivery").build()),
            Map.entry("deliveryRequiredBy", RunnerEntityMapping.builder().tableName("deliveryDetails").dataType(LocalDateTime.class).fieldName("requiredBy").build()),
            Map.entry("pickupEstimated", RunnerEntityMapping.builder().tableName("pickupDetails").dataType(LocalDateTime.class).fieldName("estimatedPickupOrDelivery").build()),
            Map.entry("pickupActual", RunnerEntityMapping.builder().tableName("pickupDetails").dataType(LocalDateTime.class).fieldName("actualPickupOrDelivery").build()),
            Map.entry("pickupRequiredBy", RunnerEntityMapping.builder().tableName("pickupDetails").dataType(LocalDateTime.class).fieldName("requiredBy").build()),
            Map.entry("screeningStatus", RunnerEntityMapping.builder().tableName("additionalDetails").dataType(String.class).fieldName("screeningStatus").isContainsText(true).build()),
            Map.entry("paidPlace", RunnerEntityMapping.builder().tableName("additionalDetails").dataType(Long.class).fieldName("paidPlace").build()),
            Map.entry("placeOfIssue", RunnerEntityMapping.builder().tableName("additionalDetails").dataType(Long.class).fieldName("placeOfIssue").build()),
            Map.entry("dateOfIssue", RunnerEntityMapping.builder().tableName("additionalDetails").dataType(LocalDateTime.class).fieldName("dateOfIssue").build()),
            Map.entry("dateOfReceipt", RunnerEntityMapping.builder().tableName("additionalDetails").dataType(LocalDateTime.class).fieldName("dateOfReceipt").build()),
            Map.entry("goodsCo", RunnerEntityMapping.builder().tableName("additionalDetails").dataType(String.class).fieldName("goodsCo").build()),
            Map.entry("BOEDate", RunnerEntityMapping.builder().tableName("additionalDetails").dataType(LocalDateTime.class).fieldName("BOEDate").build()),
            Map.entry("boeNumber", RunnerEntityMapping.builder().tableName("additionalDetails").dataType(String.class).fieldName("BOENumber").isContainsText(true).build()),
            Map.entry("shippingLine", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(String.class).fieldName("shippingLine").isContainsText(true).build()),
            Map.entry("vessel", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(String.class).fieldName("vessel").build()),
            Map.entry("voyage", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(String.class).fieldName("voyage").build()),
            Map.entry("origin", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(String.class).fieldName("origin").build()),
            Map.entry("destination", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(String.class).fieldName("destination").build()),
            Map.entry("originPort", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(String.class).fieldName("originPort").build()),
            Map.entry("destinationPort", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(String.class).fieldName("destinationPort").build()),
            Map.entry("eta", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(LocalDateTime.class).fieldName("eta").build()),
            Map.entry("etd", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(LocalDateTime.class).fieldName("etd").build()),
            Map.entry("ata", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(LocalDateTime.class).fieldName("ata").build()),
            Map.entry("atd", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(LocalDateTime.class).fieldName("atd").build()),
            Map.entry("weight", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(BigDecimal.class).fieldName("weight").build()),
            Map.entry("weightUnit", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("weightUnit").build()),
            Map.entry("volume", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(BigDecimal.class).fieldName("volume").build()),
            Map.entry("volumeUnit", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("volumeUnit").build()),
            Map.entry("volumetricWeight", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(BigDecimal.class).fieldName("volumetricWeight").build()),
            Map.entry("volumetricWeightUnit", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("volumetricWeightUnit").build()),
            Map.entry("chargable", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(BigDecimal.class).fieldName("chargable").build()),
            Map.entry("chargeableUnit", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("chargeableUnit").build()),
            Map.entry("netWeight", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(BigDecimal.class).fieldName("netWeight").build()),
            Map.entry("netWeightUnit", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("netWeightUnit").build()),
            Map.entry("noOfPacks", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(Integer.class).fieldName("noOfPacks").build()),
            Map.entry("packsUnit", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("packsUnit").build()),
            Map.entry("innerPacks", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(Integer.class).fieldName("innerPacks").build()),
            Map.entry("innerPackUnit", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("innerPackUnit").build()),
            Map.entry("jobStatus", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("jobStatus").build()),
            Map.entry("containerNumber", RunnerEntityMapping.builder().tableName("containersList").dataType(String.class).fieldName("containerNumber").build()),
            Map.entry("containerCode", RunnerEntityMapping.builder().tableName("containersList").dataType(String.class).fieldName("containerCode").build()),
            Map.entry("id", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(Long.class).fieldName("id").build()),
            Map.entry("consolidationNumber", RunnerEntityMapping.builder().tableName("consolidationList").dataType(String.class).fieldName("consolidationNumber").build()),
            Map.entry("orderNumber", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("orderManagementId").build()),
            Map.entry("referenceNumber", RunnerEntityMapping.builder().tableName("referenceNumbersList").dataType(String.class).fieldName("referenceNumber").build()),
            Map.entry("activityType", RunnerEntityMapping.builder().tableName("additionalDetails").dataType(String.class).fieldName("activityType").build()),
            Map.entry("goodsCO", RunnerEntityMapping.builder().tableName("additionalDetails").dataType(String.class).fieldName("goodsCO").build()),
            Map.entry("route", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("route").build()),
            Map.entry("cargoFinanceBooking", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(Boolean.class).fieldName("cargoFinanceBooking").build()),
            Map.entry("isCmsHBLSent", RunnerEntityMapping.builder().tableName("additionalDetails").dataType(Boolean.class).fieldName("isCmsHBLSent").build()),
            Map.entry("orderManagementId", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("orderManagementId").isContainsText(true).build()),
            Map.entry("flightNumber", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(String.class).fieldName("flightNumber").build())
    );

    @Override
    @Transactional
    public List<ShipmentDetails> createTestShipment(Integer count) {
        List<ShipmentDetails> response = new ArrayList<>();
        /**
         * BL details
         * Measurements
         * carrier
         * pickup
         * Delivery* *
         * Shipment details
         * Parties*
         * * * * * * *
         * * * */

        for (int i = 0; i < count; i++) {

            ShipmentDetails shipmentDetail = createShipmentData();
            /**
             * Carrier Details*
             */
            CarrierDetails carrierDetail = createCarrier();
            shipmentDetail.setCarrierDetails(carrierDetail);

            shipmentDetail = shipmentDao.save(shipmentDetail, false);
            afterSave(shipmentDetail, true);
        }

        return response;
    }

    @Override
    @Transactional
    public ResponseEntity<?> fetchShipments(CommonRequestModel commonRequestModel) {
        ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
        request.setIncludeTbls(Arrays.asList("additionalDetails", "client", "consigner", "consignee", "carrierDetails", "pickupDetails", "deliveryDetails"));
        Pair<Specification<ShipmentDetails>, Pageable> tuple = fetchData(request, ShipmentDetails.class, tableNames);
        Page<ShipmentDetails> shipmentDetailsPage = shipmentDao.findAll(tuple.getLeft(), tuple.getRight());
        return ResponseHelper.buildListSuccessResponse(
                convertEntityListToDtoList(shipmentDetailsPage.getContent()),
                shipmentDetailsPage.getTotalPages(),
                shipmentDetailsPage.getTotalElements());
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<ShipmentDetails> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(shipmentDetail -> {
            ShipmentListResponse response = modelMapper.map(shipmentDetail, ShipmentListResponse.class);
            containerCountUpdate(shipmentDetail, response);
            setEventData(shipmentDetail, response);
            if (shipmentDetail.getStatus() != null && shipmentDetail.getStatus() < ShipmentStatus.values().length)
                response.setShipmentStatus(ShipmentStatus.values()[shipmentDetail.getStatus()].toString());
            responseList.add(response);
        });
        try {
            masterDataUtils.setLocationData(responseList, EntityTransferConstants.LOCATION_SERVICE_GUID);
            masterDataUtils.setContainerTeuData(lst, responseList);
            setBillingData(lst, responseList);
        }
        catch (Exception ex) {
            log.error("Request: {} || Error occured for event: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), IntegrationType.MASTER_DATA_FETCH_FOR_SHIPMENT_LIST, ex.getLocalizedMessage());
        }
        return responseList;
    }

    private void containerCountUpdate(ShipmentDetails shipmentDetail, ShipmentListResponse response) {
        Long container20Count = 0L;
        Long container40Count = 0L;
        Long container20GPCount = 0L;
        Long container20RECount = 0L;
        Long container40GPCount = 0L;
        Long container40RECount = 0L;
        Set<String> containerNumber = new HashSet<>();
        if (shipmentDetail.getContainersList() != null) {
            for (Containers container : shipmentDetail.getContainersList()) {
                if(container.getContainerCode() != null) {
                    if (container.getContainerCode().contains(Constants.Cont20)) {
                        ++container20Count;
                    } else if (container.getContainerCode().contains(Constants.Cont40)) {
                        ++container40Count;
                    } else if (container.getContainerCode().equals(Constants.Cont20GP)) {
                        ++container20GPCount;
                    } else if (container.getContainerCode().equals(Constants.Cont20RE)) {
                        ++container20RECount;
                    } else if (container.getContainerCode().equals(Constants.Cont40GP)) {
                        ++container40GPCount;
                    } else if (container.getContainerCode().equals(Constants.Cont40RE)) {
                        ++container40RECount;
                    }
                }
                if (StringUtility.isNotEmpty(container.getContainerNumber())) {
                    containerNumber.add(container.getContainerNumber());
                }
            }
//            container20Count = shipmentDetail.getContainersList().stream().filter(container -> container.getContainerCode() != null && container.getContainerCode().contains(Constants.Cont20)).count();
//            container40Count = shipmentDetail.getContainersList().stream().filter(container -> container.getContainerCode() != null && container.getContainerCode().contains(Constants.Cont40)).count();
//            container20GPCount = shipmentDetail.getContainersList().stream().filter(container -> container.getContainerCode() != null && container.getContainerCode().equals(Constants.Cont20GP)).count();
//            container20RECount = shipmentDetail.getContainersList().stream().filter(container -> container.getContainerCode() != null && container.getContainerCode().equals(Constants.Cont20RE)).count();
//            container40GPCount = shipmentDetail.getContainersList().stream().filter(container -> container.getContainerCode() != null && container.getContainerCode().equals(Constants.Cont40GP)).count();
//            container40RECount = shipmentDetail.getContainersList().stream().filter(container -> container.getContainerCode() != null && container.getContainerCode().equals(Constants.Cont40RE)).count();
        }
        response.setContainer20Count(container20Count);
        response.setContainer40Count(container40Count);
        response.setContainer20GPCount(container20GPCount);
        response.setContainer20RECount(container20RECount);
        response.setContainer40GPCount(container40GPCount);
        response.setContainer40RECount(container40RECount);
        response.setContainerNumbers(containerNumber);
    }

    private void setEventData(ShipmentDetails shipmentDetail, ShipmentListResponse response) {
        if (shipmentDetail.getEventsList() != null) {
            for (Events events : shipmentDetail.getEventsList()) {
                if (StringUtility.isNotEmpty(events.getEventCode())) {
                    if (events.getEventCode().equalsIgnoreCase(Constants.INVGNTD)) {
                        response.setInvoiceDate(events.getActual());
                    } else if (events.getEventCode().equalsIgnoreCase(Constants.TAXSG)) {
                        response.setTaxDate(events.getActual());
                    } else if (events.getEventCode().equalsIgnoreCase(Constants.CSEDI)) {
                        response.setCustomsFilingDate(events.getActual());
                    } else if (events.getEventCode().equalsIgnoreCase(Constants.AMSEDI)) {
                        response.setAmsFilingDate(events.getActual());
                    }
                }
            }
        }
    }

    private void setBillingData(List<ShipmentDetails> shipmentDetails, List<IRunnerResponse> responseList) {

        Map<Long, ShipmentListResponse> dataMap = new HashMap<>();
        for (IRunnerResponse response : responseList){
            dataMap.put(((ShipmentListResponse)response).getId(), (ShipmentListResponse)response);
        }

        if(shipmentDetails != null && shipmentDetails.size() > 0) {
            List<UUID> guidsList = new ArrayList<>();
            for (ShipmentDetails details: shipmentDetails) {
                guidsList.add(details.getGuid());
            }
            if(guidsList.size() > 0) {
                ShipmentBillingListRequest shipmentBillingListRequest = new ShipmentBillingListRequest();
                shipmentBillingListRequest.setGuidsList(guidsList);
                ShipmentBillingListResponse shipmentBillingListResponse = v1Service.fetchShipmentBillingData(shipmentBillingListRequest);
                if(shipmentBillingListResponse.getData() != null && !shipmentBillingListResponse.getData().isEmpty()) {
                    for (ShipmentDetails details: shipmentDetails) {
                        if(shipmentBillingListResponse.getData().get(details.getGuid()) != null) {
                            ShipmentBillingListResponse.BillingData billingData = shipmentBillingListResponse.getData().get(details.getGuid());
                            ShipmentListResponse shipmentListResponse = dataMap.get(details.getId());

                            shipmentListResponse.setBillStatus(billingData.getBillStatus());
                            shipmentListResponse.setJobStatus(billingData.getJobStatus());
                            shipmentListResponse.setTotalEstimatedCost(billingData.getTotalEstimatedCost());
                            shipmentListResponse.setTotalEstimatedRevenue(billingData.getTotalEstimatedRevenue());
                            shipmentListResponse.setTotalEstimatedProfit(billingData.getTotalEstimatedProfit());
                            shipmentListResponse.setTotalEstimatedProfitPercent(billingData.getTotalEstimatedProfitPercent());
                            shipmentListResponse.setTotalCost(billingData.getTotalCost());
                            shipmentListResponse.setTotalRevenue(billingData.getTotalRevenue());
                            shipmentListResponse.setTotalProfit(billingData.getTotalProfit());
                            shipmentListResponse.setTotalProfitPercent(billingData.getTotalProfitPercent());
                            shipmentListResponse.setTotalPostedCost(billingData.getTotalPostedCost());
                            shipmentListResponse.setTotalPostedRevenue(billingData.getTotalPostedRevenue());
                            shipmentListResponse.setTotalPostedProfit(billingData.getTotalPostedProfit());
                            shipmentListResponse.setTotalPostedProfitPercent(billingData.getTotalPostedProfitPercent());
                            shipmentListResponse.setWayBillNumber(billingData.getWayBillNumber());
                        }
                    }

                }
            }
        }
    }


    private List<Parties> createParties(ShipmentDetails shipmentDetails) {
        List<Parties> parties = new ArrayList<>();
        int random = new Random().nextInt(100);
        for (String partyType : PARTY_TYPE) {
            Parties party = Parties.builder()
                    .type(partyType).orgCode(generateString(7)).addressCode(generateString(7))
                    .orgData(ORG).addressData(ADDRESS)
                    .entityId(shipmentDetails.getId()).entityType("SHIPMENT")
                    .build();
            party.setTenantId(1);
            parties.add(party);
        }
        parties = partiesDao.saveAll(parties);
        return parties;
    }

    private CarrierDetails createCarrier() {
        int random = new Random().nextInt(100);
        CarrierDetails carrier = CarrierDetails.builder()
                .shippingLine(SHIPPING_LINE.get(random % SHIPPING_LINE.size()))
                .vessel(generateString(5)).voyage(generateString(5)).origin(LOCATIONS.get(random % LOCATIONS.size())).destination(LOCATIONS.get(random % LOCATIONS.size()))
                .eta(LocalDateTime.now()).etd(LocalDateTime.now()).ata(LocalDateTime.now()).atd(LocalDateTime.now())
                .build();
        carrier.setTenantId(1);
        return carrierDao.save(carrier);
    }


    private ShipmentDetails createShipmentData() {
        int random = new Random().nextInt(100);
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().direction(DIRECTIONS.get(random % DIRECTIONS.size())).status(1)
                .source(SOURCE.get(random % SOURCE.size())).transportMode(TRANSPORT_MODES.get(random % TRANSPORT_MODES.size())).shipmentType(SHIPMENT_TYPE.get(random % SHIPMENT_TYPE.size()))
                .houseBill(generateString(10)).masterBill(generateString(10)).bookingReference(generateString(10)).consolRef(generateString(10)).paymentTerms(generateString(3))
                .goodsDescription(generateString(10)).additionalTerms(generateString(10))
                .build();
        shipmentDetails.setTenantId(1);
        return shipmentDetails;
    }

    private String generateString(int length) {
        String SALTCHARS = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890";
        StringBuilder salt = new StringBuilder();
        Random rnd = new Random();
        while (salt.length() < length) {
            int index = (int) (rnd.nextFloat() * SALTCHARS.length());
            salt.append(SALTCHARS.charAt(index));
        }
        return salt.toString();
    }

    @Transactional
    private ResponseEntity<?> createFromBooking(CommonRequestModel commonRequestModel)
    {
        ShipmentRequest request = (ShipmentRequest) commonRequestModel.getData();
        if (request == null) {
            log.error("Request is null for Shipment Create From Booking with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        ShipmentDetails shipmentDetails = jsonHelper.convertValue(request, ShipmentDetails.class);
        try {
            if(request.getConsolidationList() != null)
                shipmentDetails.setConsolidationList(jsonHelper.convertValueToList(request.getConsolidationList(), ConsolidationDetails.class));
            if(request.getContainersList() != null)
                shipmentDetails.setContainersList(jsonHelper.convertValueToList(request.getContainersList(), Containers.class));
            shipmentDetails = getShipment(shipmentDetails);
            Long shipmentId = shipmentDetails.getId();
            List<Packing> updatedPackings = new ArrayList<>();
            if (request.getPackingList() != null) {
                updatedPackings = packingDao.saveEntityFromShipment(jsonHelper.convertValueToList(request.getPackingList(), Packing.class), shipmentId);
                shipmentDetails.setPackingList(updatedPackings);
            }
            List<RoutingsRequest> routingsRequest = request.getRoutingsList();
            if (routingsRequest != null)
                shipmentDetails.setRoutingsList(routingsDao.saveEntityFromShipment(jsonHelper.convertValueToList(routingsRequest, Routings.class), shipmentId));
            Hbl hbl = null;
            if(shipmentDetails.getContainersList() != null && shipmentDetails.getContainersList().size() > 0) {
                hbl = hblService.checkAllContainerAssigned(shipmentId, shipmentDetails.getContainersList(), updatedPackings);
            }

            List<NotesRequest> notesRequest = request.getNotesList();
            for(NotesRequest req : notesRequest) {
                req.setEntityId(shipmentId);
            }
            if (notesRequest != null) {
                for(NotesRequest req : notesRequest) {
                    notesDao.save(jsonHelper.convertValue(req, Notes.class));
                }
            }

            afterSave(shipmentDetails, true);
            try {
                shipmentSync.sync(shipmentDetails, null);
            } catch (Exception e){
                log.error("Error performing sync on shipment entity, {}", e);
            }
            if(hbl != null) {
                try {
                    hblSync.sync(hbl);
                }
                catch (Exception e) {
                    log.error("Error performing sync on hbl entity, {}", e);
                }
            }
            auditLogService.addAuditLog(
                AuditLogMetaData.builder()
                        .newData(shipmentDetails)
                        .prevData(null)
                        .parent(ShipmentDetails.class.getSimpleName())
                        .parentId(shipmentDetails.getId())
                        .operation(DBOperationType.CREATE.name()).build()
            );
        } catch (Exception e) {
            log.error(e.getMessage());
            throw new RuntimeException(e);
        }
        return ResponseHelper.buildSuccessResponse(jsonHelper.convertValue(shipmentDetails, ShipmentDetailsResponse.class));
    }

    @Override
    @Transactional
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) {
        //ExecutorService executorService = Executors.newFixedThreadPool(100);

        ShipmentRequest request = (ShipmentRequest) commonRequestModel.getData();
        if (request == null) {
            log.error("Request is null for Shipment Create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

        ShipmentDetails shipmentDetails = jsonHelper.convertCreateValue(request, ShipmentDetails.class);
        //AdditionalDetails additionalDetails = jsonHelper.convertCreateValue(request.getAdditionalDetails(), AdditionalDetails.class);
        //CarrierDetails carrierDetails = jsonHelper.convertCreateValue(request.getCarrierDetails(), CarrierDetails.class);
        if(request.getConsolidationList() != null)
            shipmentDetails.setConsolidationList(jsonHelper.convertValueToList(request.getConsolidationList(), ConsolidationDetails.class));

        List<Long> tempConsolIds = new ArrayList<>();

        List<ConsolidationDetailsRequest> consolidationDetailsRequests = request.getConsolidationList();
        if (consolidationDetailsRequests != null) {
            for (ConsolidationDetailsRequest consolidation : consolidationDetailsRequests) {
                if (consolidation.getId() != null) {
                    tempConsolIds.add(consolidation.getId());
                }
            }
        }

        try {
            if (Objects.isNull(shipmentDetails.getSourceTenantId()))
                shipmentDetails.setSourceTenantId(Long.valueOf(UserContext.getUser().TenantId));
            List<ShipmentSettingsDetails> shipmentSettingsDetailsList = shipmentSettingsDao.getSettingsByTenantIds(List.of(TenantContext.getCurrentTenant()));
            ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
            if(shipmentSettingsDetailsList.size() > 0)
                shipmentSettingsDetails = shipmentSettingsDetailsList.get(0);
//            if (request.getConsolidationList() != null) {
//                List<ConsolidationDetailsRequest> consolRequest = request.getConsolidationList();
//                List<ConsolidationDetails> consolList = consolidationDetailsDao.saveAll(commonUtils.convertToCreateEntityList(consolRequest, ConsolidationDetails.class));
//                shipmentDetails.setConsolidationList(consolList);
//            }
            List<PackingRequest> packingRequest = request.getPackingList();
            List<ContainerRequest> containerRequest = request.getContainersList();
            if(shipmentDetails.getContainerAutoWeightVolumeUpdate() != null && shipmentDetails.getContainerAutoWeightVolumeUpdate().booleanValue() && packingRequest != null) {
                containerRequest = calculateAutoContainerWeightAndVolume(containerRequest, packingRequest);
            }
            Long consolidationId = null;
            if(shipmentDetails.getConsolidationList() != null && shipmentDetails.getConsolidationList().size() > 0)
                consolidationId = shipmentDetails.getConsolidationList().get(0).getId();
            List<Containers> updatedContainers = new ArrayList<>();
            if (request.getContainersList() != null) {
                for (ContainerRequest containerRequest1 : containerRequest) {
                    containerRequest1.setConsolidationId(consolidationId);
                }
                updatedContainers = containerDao.saveAll(commonUtils.convertToCreateEntityList(containerRequest, Containers.class));
                shipmentDetails.setContainersList(updatedContainers);
            }

            beforeSave(shipmentDetails);
            shipmentDetails = getShipment(shipmentDetails);
            Long shipmentId = shipmentDetails.getId();

            if(shipmentDetails.getContainersList() != null && !shipmentDetails.getContainersList().isEmpty())
            {
                for (Containers container: shipmentDetails.getContainersList()) {
                    try {
                        auditLogService.addAuditLog(
                                AuditLogMetaData.builder()
                                        .newData(container)
                                        .prevData(null)
                                        .parent(ShipmentDetails.class.getSimpleName())
                                        .parentId(shipmentId)
                                        .operation(DBOperationType.CREATE.name()).build()
                        );
                    } catch (IllegalAccessException | NoSuchFieldException | JsonProcessingException | InvocationTargetException | NoSuchMethodException e) {
                        log.error(e.getMessage());
                    }
                }
            }

//            attachConsolidations(shipmentId, tempConsolIds);

            List<Packing> updatedPackings = new ArrayList<>();
            if (packingRequest != null) {
                packingRequest = setPackingDetails(updatedContainers, packingRequest, shipmentDetails.getTransportMode(), consolidationId);
                updatedPackings = packingDao.saveEntityFromShipment(commonUtils.convertToCreateEntityList(packingRequest, Packing.class), shipmentId);
                shipmentDetails.setPackingList(updatedPackings);
            }

            List<BookingCarriageRequest> bookingCarriageRequest = request.getBookingCarriagesList();
            if (bookingCarriageRequest != null)
                shipmentDetails.setBookingCarriagesList(bookingCarriageDao.saveEntityFromShipment(commonUtils.convertToCreateEntityList(bookingCarriageRequest, BookingCarriage.class), shipmentId));

            List<ELDetailsRequest> elDetailsRequest = request.getElDetailsList();
            if (elDetailsRequest != null)
                shipmentDetails.setElDetailsList(elDetailsDao.saveEntityFromShipment(commonUtils.convertToCreateEntityList(elDetailsRequest, ELDetails.class), shipmentId));

            List<EventsRequest> eventsRequest = request.getEventsList();
            if (eventsRequest != null)
                shipmentDetails.setEventsList(eventDao.saveEntityFromOtherEntity(commonUtils.convertToCreateEntityList(eventsRequest, Events.class), shipmentId, Constants.SHIPMENT));

            List<FileRepoRequest> fileRepoRequest = request.getFileRepoList();
            if (fileRepoRequest != null)
                shipmentDetails.setFileRepoList(fileRepoDao.saveEntityFromOtherEntity(commonUtils.convertToCreateEntityList(fileRepoRequest, FileRepo.class), shipmentId, Constants.SHIPMENT));

            List<JobRequest> jobRequest = request.getJobsList();
            if (jobRequest != null)
                shipmentDetails.setJobsList(jobDao.saveEntityFromShipment(commonUtils.convertToCreateEntityList(jobRequest, Jobs.class), shipmentId));

            List<NotesRequest> notesRequest = request.getNotesList();
            if (notesRequest != null)
                shipmentDetails.setNotesList(notesDao.saveEntityFromOtherEntity(commonUtils.convertToCreateEntityList(notesRequest, Notes.class), shipmentId, Constants.SHIPMENT));

            List<ReferenceNumbersRequest> referenceNumbersRequest = request.getReferenceNumbersList();
            if (referenceNumbersRequest != null)
                shipmentDetails.setReferenceNumbersList(referenceNumbersDao.saveEntityFromShipment(commonUtils.convertToCreateEntityList(referenceNumbersRequest, ReferenceNumbers.class), shipmentId));

            List<RoutingsRequest> routingsRequest = request.getRoutingsList();
            if (routingsRequest != null)
                shipmentDetails.setRoutingsList(routingsDao.saveEntityFromShipment(commonUtils.convertToCreateEntityList(routingsRequest, Routings.class), shipmentId));

            List<ServiceDetailsRequest> serviceDetailsRequest = request.getServicesList();
            if (serviceDetailsRequest != null)
                shipmentDetails.setServicesList(serviceDetailsDao.saveEntityFromShipment(commonUtils.convertToCreateEntityList(serviceDetailsRequest, ServiceDetails.class), shipmentId));

            List<PartiesRequest> shipmentAddressRequest = request.getShipmentAddresses();
            if (shipmentAddressRequest != null)
                shipmentDetails.setShipmentAddresses(partiesDao.saveEntityFromOtherEntity(commonUtils.convertToCreateEntityList(shipmentAddressRequest, Parties.class), shipmentId, Constants.SHIPMENT_ADDRESSES));

            // Create Shipment Route in Console for Auto Attach Consolidation;
            if (request.getReplaceConsoleRoute() != null && request.getReplaceConsoleRoute()){
                createShipmentRouteInConsole(request);
            }

            Hbl hbl = null;
            ConsolidationDetails consolidationDetails = null;
            if(updatedContainers.size() > 0) {
                hbl = hblService.checkAllContainerAssigned(shipmentId, updatedContainers, updatedPackings);
                if((tempConsolIds == null || tempConsolIds.size() == 0) && (shipmentSettingsDetails.getIsShipmentLevelContainer() == null || !shipmentSettingsDetails.getIsShipmentLevelContainer())) {
                    consolidationDetails = createConsolidation(shipmentDetails, updatedContainers);
                }
            }
            afterSave(shipmentDetails, true);
            updateMasterBill(shipmentDetails, null);
            updateLinkedShipmentData(shipmentDetails, null);
            // Create events on basis of shipment status Confirmed/Created
            autoGenerateEvents(shipmentDetails, null);
            try {
                shipmentSync.sync(shipmentDetails, null);
            } catch (Exception e){
                log.error("Error performing sync on shipment entity, {}", e);
            }
            if(hbl != null) {
                try {
                    hblSync.sync(hbl);
                }
                catch (Exception e) {
                    log.error("Error performing sync on hbl entity, {}", e);
                }
            }
            if(consolidationDetails != null) {
                try {
                    consolidationSync.sync(consolidationDetails);
                } catch (Exception e) {
                    log.error("Error performing sync on consol entity, {}", e);
                }
            }

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(shipmentDetails)
                            .prevData(null)
                            .parent(ShipmentDetails.class.getSimpleName())
                            .parentId(shipmentDetails.getId())
                            .operation(DBOperationType.CREATE.name()).build()
            );

        } catch (Exception e) {
            log.error(e.getMessage());
            //TransactionAspectSupport.currentTransactionStatus().setRollbackOnly();
            throw new RuntimeException(e);
        }


//        CompletableFuture.allOf(createCallToAdditionalDetails, createCallToContainers, createCallToPackings, createCallToBookingCarriages, createCallToElDetails, createCallToEvents, createCallToFileRepos, createCallToJobs, createCallToNotes, createCallToReferenceNumbers, createCallToRoutings, createCallToServiceDetails, createCallToPickupDelivery, createCallToParties, createCallToCarrierDetails).join();
//        executorService.shutdownNow();
        return ResponseHelper.buildSuccessResponse(jsonHelper.convertValue(shipmentDetails, ShipmentDetailsResponse.class));
    }

    ShipmentDetails getShipment(ShipmentDetails shipmentDetails) {
        if(shipmentDetails.getShipmentId() == null){
            this.currentShipment = shipmentDetails;
            shipmentDetails.setShipmentId(generateShipmentId());
    }
        shipmentDetails = shipmentDao.save(shipmentDetails, false);
        return shipmentDetails;
        //shipmentDetails = shipmentDao.findById(shipmentDetails.getId()).get();
    }


    private void createPartiesAsync(ShipmentDetails shipmentDetails, List<PartiesRequest> partiesRequest) {
        partiesRequest.forEach(parties -> {
            createParties(shipmentDetails, parties);
        });
    }

    private void createServiceDetailsAsync(ShipmentDetails shipmentDetails, List<ServiceDetailsRequest> serviceDetailsRequest) {
        serviceDetailsRequest.forEach(serviceDetails -> {
            createServiceDetail(shipmentDetails, serviceDetails);
        });
    }

    private void createRoutingsAsync(ShipmentDetails shipmentDetails, List<RoutingsRequest> routingsRequest) {
        routingsRequest.forEach(routing -> {
            createRouting(shipmentDetails, routing);
        });
    }

    private void createReferenceNumbersAsync(ShipmentDetails shipmentDetails, List<ReferenceNumbersRequest> referenceNumbersRequest) {
        referenceNumbersRequest.forEach(referenceNumber -> {
            createReferenceNumber(shipmentDetails, referenceNumber);
        });
    }

    private void createNotesAsync(ShipmentDetails shipmentDetails, List<NotesRequest> notesRequest) {
        notesRequest.forEach(notes -> {
            createNote(shipmentDetails, notes);
        });
    }

    private void createJobsAsync(ShipmentDetails shipmentDetails, List<JobRequest> jobRequest) {
        jobRequest.forEach(jobs -> {
            createJob(shipmentDetails, jobs);
        });
    }

    private void createFileRepoAsync(ShipmentDetails shipmentDetails, List<FileRepoRequest> fileRepoRequest) {
        fileRepoRequest.forEach(fileRepo -> {
            createFileRepo(shipmentDetails, fileRepo);
        });
    }

    private void createEventsAsync(ShipmentDetails shipmentDetails, List<EventsRequest> eventsRequest) {
        eventsRequest.forEach(event -> {
            createEvent(shipmentDetails, event);
        });
    }

    private void createElDetailsAsync(ShipmentDetails shipmentDetails, List<ELDetailsRequest> elDetailsRequest) {
        elDetailsRequest.forEach(elDetails -> {
            createElDetail(shipmentDetails, elDetails);
        });
    }

    private void createBookingCarriagesAsync(ShipmentDetails shipmentDetails, List<BookingCarriageRequest> bookingCarriageRequest) {
        bookingCarriageRequest.forEach(booking -> {
            createbookingCarriage(shipmentDetails, booking);
        });
    }

    private void createPackingsAsync(ShipmentDetails shipmentDetails, List<PackingRequest> packingRequest) {
        packingRequest.forEach(packing -> {
            createPacking(shipmentDetails, packing);
        });

    }

    @Transactional
    public void createbookingCarriage(ShipmentDetails shipmentDetails, BookingCarriageRequest bookingCarriageRequest) {
        bookingCarriageRequest.setShipmentId(shipmentDetails.getId());
        bookingCarriageDao.save(objectMapper.convertValue(bookingCarriageRequest, BookingCarriage.class));
    }

    @Transactional
    public void createElDetail(ShipmentDetails shipmentDetails, ELDetailsRequest elDetailsRequest) {
        elDetailsRequest.setShipmentId(shipmentDetails.getId());
        elDetailsDao.save(objectMapper.convertValue(elDetailsRequest, ELDetails.class));
    }

    @Transactional
    public void createEvent(ShipmentDetails shipmentDetails, EventsRequest eventsRequest) {
        eventsRequest.setEntityId(shipmentDetails.getId());
        eventsRequest.setEntityType(Constants.SHIPMENT);
        eventDao.save(objectMapper.convertValue(eventsRequest, Events.class));
    }

    @Transactional
    public void createFileRepo(ShipmentDetails shipmentDetails, FileRepoRequest fileRepoRequest) {
        fileRepoRequest.setEntityId(shipmentDetails.getId());
        fileRepoRequest.setEntityType(Constants.SHIPMENT);
        fileRepoDao.save(objectMapper.convertValue(fileRepoRequest, FileRepo.class));
    }

    @Transactional
    public void createJob(ShipmentDetails shipmentDetails, JobRequest jobRequest) {
        jobRequest.setShipmentId(shipmentDetails.getId());
        jobDao.save(objectMapper.convertValue(jobRequest, Jobs.class));
    }

    @Transactional
    public void createNote(ShipmentDetails shipmentDetails, NotesRequest notesRequest) {
        notesRequest.setEntityId(shipmentDetails.getId());
        notesRequest.setEntityType(Constants.SHIPMENT);
        notesDao.save(objectMapper.convertValue(notesRequest, Notes.class));
    }

    @Transactional
    public void createParties(ShipmentDetails shipmentDetails, PartiesRequest partiesRequest) {
        partiesRequest.setEntityId(shipmentDetails.getId());
        partiesRequest.setEntityType("SHIPMENT");
        packingDao.save(objectMapper.convertValue(partiesRequest, Packing.class));
    }

    @Transactional
    public void createReferenceNumber(ShipmentDetails shipmentDetails, ReferenceNumbersRequest referenceNumbersRequest) {
        referenceNumbersRequest.setShipmentId(shipmentDetails.getId());
        referenceNumbersDao.save(objectMapper.convertValue(referenceNumbersRequest, ReferenceNumbers.class));
    }

    @Transactional
    public void createPacking(ShipmentDetails shipmentDetails, PackingRequest packingRequest) {
        packingRequest.setShipmentId(shipmentDetails.getId());
        packingDao.save(objectMapper.convertValue(packingRequest, Packing.class));
    }

    @Transactional
    public void createRouting(ShipmentDetails shipmentDetails, RoutingsRequest routingsRequest) {
        routingsRequest.setShipmentId(shipmentDetails.getId());
        routingsDao.save(objectMapper.convertValue(routingsRequest, Routings.class));
    }

    @Transactional
    public void createServiceDetail(ShipmentDetails shipmentDetails, ServiceDetailsRequest serviceDetailsRequest) {
        serviceDetailsRequest.setShipmentId(shipmentDetails.getId());
        serviceDetailsDao.save(objectMapper.convertValue(serviceDetailsRequest, ServiceDetails.class));
    }

    @Transactional
    public void createAdditionalDetail(ShipmentDetails shipmentDetails, AdditionalDetails additionalDetails) {
        additionalDetailDao.save(additionalDetails);
    }

    @Transactional
    public void createCarrier(ShipmentDetails shipmentDetails, CarrierDetails carrierDetails) {
        carrierDao.save(carrierDetails);
    }


    public Optional<ShipmentDetails> retrieveByIdOrGuid(ShipmentRequest request){
        String responseMsg;

        if (request == null) {
            log.error("Request is empty for Shipment update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

        Optional<ShipmentDetails> oldEntity = Optional.ofNullable(null);

        if(request.getId()!=null){
            long id = request.getId();
            oldEntity=shipmentDao.findById(id);
            if (!oldEntity.isPresent()) {
                log.debug("Shipment Details is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }

        }

        else if(request.getGuid()!=null){
            UUID guid = request.getGuid();
            oldEntity= shipmentDao.findByGuid(guid);
            if (!oldEntity.isPresent()) {
                log.debug("Shipment Details is null for GUID {} with Request GUID {}", request.getGuid(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }

        }
        else{
            throw new RunnerException("Either Id or Guid is required");

        }
        return oldEntity;
    }
    @Override
    @Transactional
    public ResponseEntity<?> createShipmentInV2(CustomerBookingRequest customerBookingRequest) throws Exception
    {
        List<ConsolidationDetailsRequest> consolidationDetails = new ArrayList<>();
        List<ContainerRequest> containerList = new ArrayList<>();
        List<Notes> notes = notesDao.findByEntityIdAndEntityType(customerBookingRequest.getId(), "CustomerBooking");
        if(customerBookingRequest.getCargoType().equals("FCL"))
        {
            ConsolidationDetailsRequest consolidationDetailsRequest = ConsolidationDetailsRequest.builder().
                    carrierDetails(CarrierDetailRequest.builder()
                            .origin(customerBookingRequest.getCarrierDetails().getOrigin())
                            .destination(customerBookingRequest.getCarrierDetails().getDestination())
                            .shippingLine(customerBookingRequest.getCarrierDetails().getShippingLine())
                            .vessel(customerBookingRequest.getCarrierDetails().getVessel())
                            .voyage(customerBookingRequest.getCarrierDetails().getVoyage())
                            .originPort(customerBookingRequest.getCarrierDetails().getOriginPort())
                            .destinationPort(customerBookingRequest.getCarrierDetails().getDestinationPort())
                            .flightNumber(customerBookingRequest.getCarrierDetails().getFlightNumber())
                            .build()
                    ).
                    consolidationType("STD").
                    transportMode(customerBookingRequest.getTransportType()).
                    containerCategory(customerBookingRequest.getCargoType()).
                    shipmentType(customerBookingRequest.getDirection()).
                    referenceNumber(customerBookingRequest.getBookingNumber()).
                    departureDetails(ArrivalDepartureDetailsRequest.builder().
                            firstForeignPort(customerBookingRequest.getCarrierDetails().getOrigin()).
                            lastForeignPort(customerBookingRequest.getCarrierDetails().getOrigin()).
                            type("Departure").
                            build()
                    ).
                    arrivalDetails(ArrivalDepartureDetailsRequest.builder().
                            firstForeignPort(customerBookingRequest.getCarrierDetails().getDestination()).
                            lastForeignPort(customerBookingRequest.getCarrierDetails().getDestination()).
                            type("Arrival").
                            build()
                    ).
                    containersList(customerBookingRequest.getContainersList()).
                    sourceTenantId(Long.valueOf(UserContext.getUser().TenantId)).
                    build();
            ResponseEntity<?> consolidationDetailsResponse = consolidationService.create(CommonRequestModel.buildRequest(consolidationDetailsRequest));
            if(consolidationDetailsResponse != null)
            {
                ConsolidationDetailsResponse consolDetailsResponse = (ConsolidationDetailsResponse) (((RunnerResponse)consolidationDetailsResponse.getBody()).getData());
                ConsolidationDetailsRequest consolRequest = jsonHelper.convertValue(consolDetailsResponse, ConsolidationDetailsRequest.class);
                containerList = consolRequest.getContainersList();
                consolRequest.setContainersList(null);
                consolidationDetails.add(consolRequest);
            }
        }

        ShipmentRequest shipmentRequest = ShipmentRequest.builder().
                carrierDetails(CarrierDetailRequest.builder()
                        .origin(customerBookingRequest.getCarrierDetails().getOrigin())
                        .destination(customerBookingRequest.getCarrierDetails().getDestination())
                        .shippingLine(customerBookingRequest.getCarrierDetails().getShippingLine())
                        .vessel(customerBookingRequest.getCarrierDetails().getVessel())
                        .voyage(customerBookingRequest.getCarrierDetails().getVoyage())
                        .originPort(customerBookingRequest.getCarrierDetails().getOriginPort())
                        .destinationPort(customerBookingRequest.getCarrierDetails().getDestinationPort())
                        .flightNumber(customerBookingRequest.getCarrierDetails().getFlightNumber())
                        .build()
                ).
                contractId(customerBookingRequest.getContractId()).
                contractType(customerBookingRequest.getContractStatus()).
                noOfPacks(customerBookingRequest.getQuantity()).
                packsUnit(customerBookingRequest.getQuantityUnit()).
                weight(customerBookingRequest.getGrossWeight()).
                weightUnit(customerBookingRequest.getGrossWeightUnit()).
                volume(customerBookingRequest.getVolume()).
                volumeUnit(customerBookingRequest.getVolumeUnit()).
                volumetricWeight(customerBookingRequest.getWeightVolume()).
                volumetricWeightUnit(customerBookingRequest.getWeightVolumeUnit()).
                bookingReference(customerBookingRequest.getBookingNumber()).
                shipmentCreatedOn(customerBookingRequest.getBookingDate()).
                client(createPartiesRequest(customerBookingRequest.getCustomer())).
                consignee(createPartiesRequest(customerBookingRequest.getConsignee())).
                consigner(createPartiesRequest(customerBookingRequest.getConsignor())).
                additionalDetails(AdditionalDetailRequest.builder().
                        notifyParty(createPartiesRequest(customerBookingRequest.getNotifyParty())).
                        build()
                ).
                shipmentType(customerBookingRequest.getCargoType()).
                transportMode(customerBookingRequest.getTransportType()).
                direction(customerBookingRequest.getDirection()).
                jobType("STD").
                incoterms(customerBookingRequest.getIncoTerms()).
                serviceType(customerBookingRequest.getServiceMode()).
                status(4).
                fmcTlcId(customerBookingRequest.getFmcTlcId()).
                clientCountry(customerBookingRequest.getClientCountry()).
                consignorCountry(customerBookingRequest.getConsignorCountry()).
                consigneeCountry(customerBookingRequest.getConsigneeCountry()).
                notifyPartyCountry(customerBookingRequest.getNotifyPartyCountry()).
                containersList(consolidationDetails != null && consolidationDetails.size() > 0 ? containerList : null).
                packingList(customerBookingRequest.getPackingList()).
                fileRepoList(customerBookingRequest.getFileRepoList()).
                routingsList(customerBookingRequest.getRoutingList()).
                consolidationList(customerBookingRequest.getCargoType().equals("FCL") ? consolidationDetails : null).
                notesList(createNotes(notes)).
                sourceTenantId(Long.valueOf(UserContext.getUser().TenantId)).
                source("API").
                bookingType("ONLINE").
                build();

        return this.createFromBooking(CommonRequestModel.buildRequest(shipmentRequest));
    }

    private List<NotesRequest> createNotes(List<Notes> notes){
        if(notes == null) return null;
        return notes.stream().filter(Objects::nonNull).map(note ->
               NotesRequest.builder()
                        .assignedTo(note.getAssignedTo())
                        .label(note.getLabel())
                        .text(note.getText())
                        .insertUserDisplayName(note.getCreatedBy())
                        .isPublic(note.getIsPublic())
                        .insertDate(note.getCreatedAt())
                        .entityType(Constants.SHIPMENT_BOOKING)
                        .build()).collect(Collectors.toList());
    }

    private PartiesRequest createPartiesRequest(PartiesRequest party)
    {
        if(party == null)
            return null;
        return PartiesRequest.builder()
                .addressCode(party.getAddressCode())
                .addressData(party.getAddressData())
                .orgCode(party.getOrgCode())
                .orgData(party.getOrgData())
                .build();
    }

    private List<PackingRequest> setPackingDetails(List<Containers> containersList, List<PackingRequest> packingRequests, String transportMode, Long consolidationId) {
        if(containersList != null) {
            Map<String, Long> map = containersList.stream().filter(c -> c.getContainerNumber() != null).collect(Collectors.toMap(element -> element.getContainerNumber(), element -> element.getId()));
            if(packingRequests != null && packingRequests.size() > 0) {
                for (PackingRequest packingRequest : packingRequests) {
                    if(packingRequest.getContainerId() == null && !IsStringNullOrEmpty(packingRequest.getContainerNumber())) {
                        if(map.containsKey(packingRequest.getContainerNumber()))
                            packingRequest.setContainerId(map.get(packingRequest.getContainerNumber()));
                        else
                            packingRequest.setContainerId(null);
                    }
                    if(!IsStringNullOrEmpty(transportMode) && transportMode.equals(Constants.TRANSPORT_MODE_AIR)) {
                        packingRequest.setConsolidationId(consolidationId);
                    }
                }
            }
        }
        return packingRequests;
    }

    private List<ContainerRequest> calculateAutoContainerWeightAndVolume(List<ContainerRequest> containersList, List<PackingRequest> packingList) throws Exception {
        if(containersList != null && containersList.size() > 0) {
            for (ContainerRequest containers : containersList) {
                if(!IsStringNullOrEmpty(containers.getContainerNumber())) {
                    if(packingList != null) {
                        List<PackingRequest> packings = packingList.stream().filter(packing -> packing.getContainerNumber() != null && packing.getContainerNumber().equals(containers.getContainerNumber())).toList();
                        BigDecimal totalWeight = BigDecimal.ZERO;
                        BigDecimal totalVolume = BigDecimal.ZERO;
                        if(packings != null && packings.size() > 0) {
                            if(IsStringNullOrEmpty(containers.getGrossWeightUnit()))
                                containers.setGrossWeightUnit(Constants.WEIGHT_UNIT_KG);
                            if(IsStringNullOrEmpty(containers.getGrossVolumeUnit()))
                                containers.setGrossVolumeUnit(Constants.VOLUME_UNIT_M3);
                            for (PackingRequest packing : packings) {
                                if(!IsStringNullOrEmpty(packing.getWeightUnit()))
                                    totalWeight = totalWeight.add(new BigDecimal(convertUnit(Constants.MASS, packing.getWeight(), packing.getWeightUnit(), containers.getGrossWeightUnit()).toString()));
                                if(!IsStringNullOrEmpty(packing.getVolumeUnit()))
                                    totalVolume = totalVolume.add(new BigDecimal(convertUnit(Constants.VOLUME, packing.getVolume(), packing.getVolumeUnit(), containers.getGrossVolumeUnit()).toString()));
                            }
                            containers.setGrossWeight(totalWeight);
                            containers.setGrossVolume(totalVolume);
                        }
                    }
                }
            }
        }
        return containersList;
    }

    public ResponseEntity<?> calculateAutoUpdateWtVolInShipment(CommonRequestModel commonRequestModel) throws Exception {
        String responseMsg;
        try {
            AutoUpdateWtVolRequest request = (AutoUpdateWtVolRequest) commonRequestModel.getData();
            AutoUpdateWtVolResponse response = calculateShipmentWV(request);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<?> calculateWtVolInShipmentOnChanges(CommonRequestModel commonRequestModel) throws Exception {
        String responseMsg;
        try {
            AutoUpdateWtVolRequest request = (AutoUpdateWtVolRequest) commonRequestModel.getData();
            AutoUpdateWtVolResponse response = jsonHelper.convertValue(request, AutoUpdateWtVolResponse.class);
            response = calculateVW(request, response, true);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private AutoUpdateWtVolResponse calculateShipmentWV(AutoUpdateWtVolRequest request) throws Exception {
        AutoUpdateWtVolResponse response = jsonHelper.convertValue(request, AutoUpdateWtVolResponse.class);
        List<Packing> packingList = jsonHelper.convertValueToList(request.getPackingList(), Packing.class);
        List<Containers> containersList = jsonHelper.convertValueToList(request.getContainersList(), Containers.class);
//        if(request.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR)) {
//            response = calculatePacksAndPacksUnit(packingList, response);
//        }
        response = calculatePacksAndPacksUnit(packingList, response);
        response = calculateWeightAndVolumeUnit(request, packingList, response);
        ShipmentSettingsDetails shipmentSettingsDetails = shipmentSettingsDao.getSettingsByTenantIds(List.of(TenantContext.getCurrentTenant())).get(0);
        boolean isPacksPresent = packingList != null && packingList.size() > 0;
        if(shipmentSettingsDetails.getIsShipmentLevelContainer() && !request.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) && !isPacksPresent) {
            response.setContainerSummary(containerService.calculateContainerSummary(containersList, request.getTransportMode(), request.getShipmentType()));
            response = updateShipmentDetails(response, containersList);
            response = calculateVW(request, response, true);
        }
        else {
            response = calculateVW(request, response, true);
            ShipmentMeasurementDetailsDto dto = new ShipmentMeasurementDetailsDto();
            response.setPackSummary(packingService.calculatePackSummary(packingList, request.getTransportMode(), request.getShipmentType(), dto));
            if(request.getPackingList() == null || request.getPackingList().size() == 0) {
                response.setContainerSummary(containerService.calculateContainerSummary(containersList, request.getTransportMode(), request.getShipmentType()));
            }
            if(request.getTransportMode() != null && request.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA)
            && request.getShipmentType() != null && request.getShipmentType().equals(Constants.SHIPMENT_TYPE_LCL)) {
                response.setInnerPacks(dto.getInnerPacks());
                response.setInnerPackUnit(dto.getInnerPackUnit());
            }
            if(shipmentSettingsDetails.getIsShipmentLevelContainer() != null && shipmentSettingsDetails.getIsShipmentLevelContainer()
            && request.getPackingList() != null && request.getPackingList().size() > 0) {
                response.setWeight(dto.getWeight());
                response.setWeightUnit(dto.getWeightUnit());
                response.setVolume(dto.getVolume());
                response.setVolumeUnit(dto.getVolumeUnit());
                response.setNetWeight(dto.getNetWeight());
                response.setNetWeightUnit(dto.getNetWeightUnit());
                response.setNoOfPacks(dto.getNoOfPacks());
                response.setPacksUnit(dto.getPacksUnit());
            }
            else if(shipmentSettingsDetails.getIsShipmentLevelContainer() == null || !shipmentSettingsDetails.getIsShipmentLevelContainer()){
                response.setNoOfPacks(dto.getNoOfPacks());
                response.setPacksUnit(dto.getPacksUnit());
            }
        }
        V1RetrieveResponse v1RetrieveResponse = v1Service.retrieveTenantSettings();
        V1TenantSettingsResponse v1TenantSettingsResponse = modelMapper.map(v1RetrieveResponse.getEntity(), V1TenantSettingsResponse.class);
        if(v1TenantSettingsResponse.getP100Branch() && request.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA)) {
            response = calculatePacksAndPacksUnitFromContainer(response, containersList);
        }
        return response;
    }

    private AutoUpdateWtVolResponse updateShipmentDetails(AutoUpdateWtVolResponse response, List<Containers> containersList) throws Exception { // to account for updateShipmentDetails flag in v1 container summary
        double totalWeight = 0;
        double packageCount = 0;
        double tareWeight = 0;
        double totalVolume = 0;
        double totalContainerCount = 0;
        int totalPacks = 0;
        String packsUnit = "";
        String toWeightUnit = Constants.WEIGHT_UNIT_KG;
        String toVolumeUnit = Constants.VOLUME_UNIT_M3;
        ShipmentSettingsDetails shipmentSettingsDetails = shipmentSettingsDao.getSettingsByTenantIds(List.of(TenantContext.getCurrentTenant())).get(0);
        if(!IsStringNullOrEmpty(shipmentSettingsDetails.getWeightChargeableUnit()))
            toWeightUnit = shipmentSettingsDetails.getWeightChargeableUnit();
        if(!IsStringNullOrEmpty(shipmentSettingsDetails.getVolumeChargeableUnit()))
            toVolumeUnit = shipmentSettingsDetails.getVolumeChargeableUnit();
        if(containersList != null) {
            for (Containers containers : containersList) {
                double wInDef = convertUnit(Constants.MASS, containers.getGrossWeight(), containers.getGrossWeightUnit(), toWeightUnit).doubleValue();
                double tarDef = convertUnit(Constants.MASS, containers.getTareWeight(), containers.getTareWeightUnit(), toWeightUnit).doubleValue();
                double volume = convertUnit(Constants.VOLUME, containers.getGrossVolume(), containers.getGrossVolumeUnit(), toVolumeUnit).doubleValue();
                totalWeight = totalWeight + wInDef;
                tareWeight = tareWeight + tarDef;
                double noOfPackages = 0;
                if(containers.getNoOfPackages() != null)
                    noOfPackages = containers.getNoOfPackages().doubleValue();
                if(!IsStringNullOrEmpty(containers.getPacks()))
                    packageCount = packageCount + Long.parseLong(containers.getPacks());
                else
                    packageCount = packageCount + noOfPackages;
                totalVolume = totalVolume + volume;
                if(containers.getContainerCount() != null)
                    totalContainerCount = totalContainerCount + containers.getContainerCount();
                if(!IsStringNullOrEmpty(containers.getPacks()))
                    totalPacks = totalPacks + Integer.parseInt(containers.getPacks());
            }
        }
        if (containersList.size() > 0 ) {
            packsUnit = setPacksUnit(containersList, packsUnit);
        }
        response.setWeight(new BigDecimal(totalWeight));
        response.setVolume(new BigDecimal(totalVolume));
        response.setWeightUnit(toWeightUnit);
        response.setVolumeUnit(toVolumeUnit);
        response.setNoOfPacks(totalPacks == 0 ? null : String.valueOf(totalPacks));
        response.setPacksUnit(packsUnit);
        response.setNetWeight(new BigDecimal(tareWeight));
        response.setNetWeightUnit(toWeightUnit);
        return response;
    }

    private AutoUpdateWtVolResponse calculatePacksAndPacksUnitFromContainer(AutoUpdateWtVolResponse response, List<Containers> containersList) {
        if(containersList != null && containersList.size() > 0) {
            String packsUnit = "";
            long packageCount = 0;
            long totalPacks = 0;
            for (Containers container : containersList) {
                if (!IsStringNullOrEmpty(container.getPacks())) {
                    packageCount = packageCount + Integer.parseInt(container.getPacks());
                } else {
                    if(container.getNoOfPackages() != null)
                        packageCount = packageCount + container.getNoOfPackages();
                }
                if (!IsStringNullOrEmpty(container.getPacks())) {
                    totalPacks = totalPacks + Integer.parseInt(container.getPacks());
                }
            };
            packsUnit = setPacksUnit(containersList, packsUnit);
            response.setNoOfPacks(totalPacks == 0 ? null : String.valueOf(totalPacks));
            response.setPacksUnit(packsUnit);
        }
        return response;
    }

    private String setPacksUnit(List<Containers> containersList, String packsUnit) {
        String firstPacksType = containersList.get(0).getPacksType();
        boolean isSame = containersList.stream()
                .map(Containers::getPacksType)
                .allMatch(packsType -> packsType == null || packsType.equals(firstPacksType));

        if (isSame) {
            packsUnit = firstPacksType;
        } else {
            packsUnit = Constants.MPK;
        }
        return packsUnit;
    }

    private AutoUpdateWtVolResponse calculateWeightAndVolumeUnit(AutoUpdateWtVolRequest request, List<Packing> packings, AutoUpdateWtVolResponse response) throws Exception {
        BigDecimal totalWeight = BigDecimal.ZERO;
        BigDecimal totalVolume = BigDecimal.ZERO;
        if(IsStringNullOrEmpty(request.getWeightUnit()))
            response.setWeightUnit(Constants.WEIGHT_UNIT_KG);
        if(IsStringNullOrEmpty(request.getVolumeUnit()))
            response.setVolumeUnit(Constants.VOLUME_UNIT_M3);
        if(packings != null && packings.size() > 0) {
            for (Packing packing : packings) {
                if(packing.getWeight() != null && !IsStringNullOrEmpty(packing.getWeightUnit())) {
                    totalWeight = totalWeight.add(new BigDecimal(convertUnit(Constants.MASS, packing.getWeight(), packing.getWeightUnit(), response.getWeightUnit()).toString()));
                }
                if(packing.getVolume() != null && !IsStringNullOrEmpty(packing.getVolumeUnit())) {
                    totalVolume = totalVolume.add(new BigDecimal(convertUnit(Constants.VOLUME, packing.getVolume(), packing.getVolumeUnit(), response.getVolumeUnit()).toString()));
                }
            }
            response.setWeight(totalWeight);
            response.setVolume(totalVolume);
            response = calculateVW(request, response, false);
        }
        return response;
    }

    private AutoUpdateWtVolResponse calculateVW(AutoUpdateWtVolRequest request, AutoUpdateWtVolResponse response, boolean recalculateVwObInKgAndM3) throws Exception{
        if(IsStringNullOrEmpty(request.getTransportMode()))
            return response;
        if(!IsStringNullOrEmpty(response.getWeightUnit()) && !IsStringNullOrEmpty(response.getVolumeUnit())) {
            VolumeWeightChargeable vwOb = consolidationService.calculateVolumeWeight(request.getTransportMode(), response.getWeightUnit(), response.getVolumeUnit(), response.getWeight(), response.getVolume());
            response.setChargable(vwOb.getChargeable());
            if(request.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR)) {
                response.setChargable(new BigDecimal(roundOffAirShipment(response.getChargable().doubleValue())));
            }
            response.setChargeableUnit(vwOb.getChargeableUnit());
            if(request.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA)) {
                if(!IsStringNullOrEmpty(request.getShipmentType()) && request.getShipmentType().equals(Constants.SHIPMENT_TYPE_LCL)) {
                    double volInM3 = convertUnit(Constants.VOLUME, response.getVolume(), response.getVolumeUnit(), Constants.VOLUME_UNIT_M3).doubleValue();
                    double wtInKg = convertUnit(Constants.MASS, response.getWeight(), response.getWeightUnit(), Constants.WEIGHT_UNIT_KG).doubleValue();
                    response.setChargable(new BigDecimal(Math.max(wtInKg/1000, volInM3)));
                    response.setChargeableUnit(Constants.VOLUME_UNIT_M3);
                    if(recalculateVwObInKgAndM3)
                        vwOb = consolidationService.calculateVolumeWeight(request.getTransportMode(), Constants.WEIGHT_UNIT_KG, Constants.VOLUME_UNIT_M3, new BigDecimal(wtInKg), new BigDecimal(volInM3));
                }
            }
            response.setVolumetricWeight(vwOb.getVolumeWeight());
            response.setVolumetricWeightUnit(vwOb.getVolumeWeightUnit());
        }
        return response;
    }

    private double roundOffAirShipment(double charge) {
        if (charge - 0.50 <= Math.floor(charge) && charge != Math.floor(charge)) {
            charge = Math.floor(charge) + 0.5;
        } else {
            charge = Math.ceil(charge);
        }
        return charge;
    }

    private AutoUpdateWtVolResponse calculatePacksAndPacksUnit(List<Packing> packings, AutoUpdateWtVolResponse response) {
        Integer totalPacks = 0;
        String tempPackingUnit = null;
        String packingUnit = null;
        if(packings != null && packings.size() > 0) {
            for (Packing packing : packings) {
                if(!IsStringNullOrEmpty(packing.getPacks()))
                    totalPacks = totalPacks + Integer.parseInt(packing.getPacks());
                if (tempPackingUnit == null) {
                    tempPackingUnit = packing.getPacksType();
                    packingUnit = packing.getPacksType();
                }
                else {
                    if(!IsStringNullOrEmpty(packing.getPacksType()) && tempPackingUnit.equals(packing.getPacksType())) {
                        packingUnit = Constants.MPK;
                    }
                }
            }
        }
        response.setNoOfPacks(totalPacks.toString());
        response.setPacksUnit(packingUnit);
        return response;
    }

    @Override
    @Transactional
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) {
        String responseMsg;
        ShipmentRequest request = (ShipmentRequest) commonRequestModel.getData();
        if (request == null) {
            log.error("Request is empty for Shipment update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
//        if (request.getId() == null) {
//            log.error("Request Id is null for Shipment update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
//        }
        // TODO- implement Validation logic

      Optional<ShipmentDetails>oldEntity =retrieveByIdOrGuid(request);

        ShipmentDetails entity = objectMapper.convertValue(request, ShipmentDetails.class);
        entity.setId(oldEntity.get().getId());
        if(entity.getGuid() != null && !oldEntity.get().getGuid().equals(entity.getGuid())) {
            throw new RunnerException("Provided GUID doesn't match with the existing one !");
        }
        if (entity.getContainersList() == null)
            entity.setContainersList(oldEntity.get().getContainersList());
        entity = shipmentDao.update(entity, false);
        afterSave(entity, false);
        try {
            shipmentSync.sync(entity, null);
        } catch (Exception e){
            log.error("Error performing sync on shipment entity, {}", e);
        }
        return ResponseHelper.buildSuccessResponse(objectMapper.convertValue(entity, ShipmentDetailsResponse.class));
    }

    @Transactional
    public ResponseEntity<?> attachConsolidations(Long shipmentId, List<Long> consolIds) {
        ShipmentDetails shipmentDetails = shipmentDao.findById(shipmentId).get();

        if (shipmentDetails != null) {
            for (Long consolId : consolIds) {
                ConsolidationDetails consolidationDetails = consolidationDetailsDao.findById(consolId).get();
                if (consolidationDetails != null) {
                    shipmentDetails.getConsolidationList().add(consolidationDetails);
                }
            }
            // TODO- remove this save
            ShipmentDetails entity = shipmentDao.save(shipmentDetails, false);
            return ResponseHelper.buildSuccessResponse(jsonHelper.convertValue(entity, ShipmentDetailsResponse.class));
        }

        return null;
    }

    @Transactional
    public ResponseEntity<?> completeUpdate(CommonRequestModel commonRequestModel) throws Exception {

        ShipmentRequest shipmentRequest = (ShipmentRequest) commonRequestModel.getData();

        List<BookingCarriageRequest> bookingCarriageRequestList = shipmentRequest.getBookingCarriagesList();
        List<PackingRequest> packingRequestList = shipmentRequest.getPackingList();
        AdditionalDetailRequest additionalDetailRequest = shipmentRequest.getAdditionalDetails();
        List<ContainerRequest> containerRequestList = shipmentRequest.getContainersList();
        List<ELDetailsRequest> elDetailsRequestList = shipmentRequest.getElDetailsList();
        List<EventsRequest> eventsRequestList = shipmentRequest.getEventsList();
        List<FileRepoRequest> fileRepoRequestList = shipmentRequest.getFileRepoList();
        List<JobRequest> jobRequestList = shipmentRequest.getJobsList();
        List<NotesRequest> notesRequestList = shipmentRequest.getNotesList();
        List<ReferenceNumbersRequest> referenceNumbersRequestList = shipmentRequest.getReferenceNumbersList();
        List<RoutingsRequest> routingsRequestList = shipmentRequest.getRoutingsList();
        List<ServiceDetailsRequest> serviceDetailsRequestList = shipmentRequest.getServicesList();
        List<PartiesRequest> shipmentAddressList = shipmentRequest.getShipmentAddresses();
        CarrierDetailRequest carrierDetailRequest = shipmentRequest.getCarrierDetails();

        // TODO- implement Validation logic

        Optional<ShipmentDetails> oldEntity = retrieveByIdOrGuid(shipmentRequest);
        long id=oldEntity.get().getId();
        Integer previousStatus = oldEntity.get().getStatus();
        if (!oldEntity.isPresent()) {
            log.debug("Shipment Details is null for Id {}", shipmentRequest.getId());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        List<Long> tempConsolIds = new ArrayList<>();
        List<Long> removedConsolIds = new ArrayList<>();

        List<ConsolidationDetailsRequest> consolidationDetailsRequests = shipmentRequest.getConsolidationList();
        if (consolidationDetailsRequests != null) {
            Set<Long> oldConsolIds = oldEntity.get().getConsolidationList().stream().map(e -> e.getId()).collect(Collectors.toSet());
            for (ConsolidationDetailsRequest consolidation : consolidationDetailsRequests) {
                if (consolidation.getId() != null) {
                    tempConsolIds.add(consolidation.getId());
                    if(oldConsolIds.contains(consolidation.getId()))
                        oldConsolIds.remove(consolidation.getId());
                }
            }
            removedConsolIds = oldConsolIds.stream().toList();
        }
        else
            tempConsolIds = oldEntity.get().getConsolidationList().stream().map(e -> e.getId()).collect(Collectors.toList());

        try {
            List<ShipmentSettingsDetails> shipmentSettingsDetailsList = shipmentSettingsDao.getSettingsByTenantIds(List.of(TenantContext.getCurrentTenant()));
            ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
            if(shipmentSettingsDetailsList.size() > 0)
                shipmentSettingsDetails = shipmentSettingsDetailsList.get(0);
            ShipmentDetails entity = objectMapper.convertValue(shipmentRequest, ShipmentDetails.class);
            entity.setId(oldEntity.get().getId());
            List<Containers> updatedContainers = new ArrayList<>();
            if(removedConsolIds != null && removedConsolIds.size() > 0) {
                List<Containers> allConsolConts = new ArrayList<>();
                for(Long consolidationId: removedConsolIds) {
                    List<Containers> containersList = containerDao.findByConsolidationId(consolidationId);
                    if(containersList != null && containersList.size() > 0) {
                        allConsolConts.addAll(containersList);
                    }
                }
                if(allConsolConts.size() > 0) {
                    if(containerRequestList == null)
                        containerRequestList = jsonHelper.convertValueToList(oldEntity.get().getContainersList(), ContainerRequest.class);
                    containerRequestList.removeIf(obj2 -> allConsolConts.stream().anyMatch(obj1 -> obj1.getId().equals(obj2.getId())));
                }
            }
            if(entity.getContainerAutoWeightVolumeUpdate() != null && entity.getContainerAutoWeightVolumeUpdate().booleanValue() && packingRequestList != null) {
                if(containerRequestList == null)
                    containerRequestList = jsonHelper.convertValueToList(oldEntity.get().getContainersList(), ContainerRequest.class);
                containerRequestList = calculateAutoContainerWeightAndVolume(containerRequestList, packingRequestList);
            }
            Long consolidationId = null;
            if(entity.getConsolidationList() != null && entity.getConsolidationList().size() > 0)
                consolidationId = entity.getConsolidationList().get(0).getId();
            if (containerRequestList != null) {
                updatedContainers = containerDao.updateEntityFromShipmentConsole(convertToEntityList(containerRequestList, Containers.class), consolidationId, id, false);
            } else {
                updatedContainers = oldEntity.get().getContainersList();
            }
            entity.setContainersList(updatedContainers);

            String oldEntityJsonString = jsonHelper.convertToJson(oldEntity.get());
            beforeSave(entity);
            entity = shipmentDao.update(entity, false);
            try {
                // audit logs
               auditLogService.addAuditLog(
                       AuditLogMetaData.builder()
                               .newData(entity)
                               .prevData(jsonHelper.readFromJson(oldEntityJsonString, ShipmentDetails.class))
                               .parent(ShipmentDetails.class.getSimpleName())
                               .parentId(entity.getId())
                               .operation(DBOperationType.UPDATE.name()).build()
               );
            }
            catch (Exception e) {
                log.error("Error creating audit service log", e);
            }

            if (bookingCarriageRequestList != null) {
                List<BookingCarriage> updatedBookingCarriages = bookingCarriageDao.updateEntityFromShipment(convertToEntityList(bookingCarriageRequestList, BookingCarriage.class), id);
                entity.setBookingCarriagesList(updatedBookingCarriages);
            }
            List<Packing> updatedPackings = new ArrayList<>();
            List<Long> deleteContainerIds = new ArrayList<>();
            if(shipmentRequest.getDeletedContainerIds() != null && shipmentRequest.getDeletedContainerIds().size() > 0) {
                deleteContainerIds = shipmentRequest.getDeletedContainerIds().stream().filter(e -> e.getId() != null).map(e -> e.getId()).collect(Collectors.toList());
                if(deleteContainerIds != null && deleteContainerIds.size() > 0) {
                    for (Long containerId : deleteContainerIds) {
                        containerDao.deleteById(containerId);
                    }
                }
            }
            if (packingRequestList != null) {
                if(updatedContainers == null)
                    updatedContainers = oldEntity.get().getContainersList();
                packingRequestList = setPackingDetails(updatedContainers, packingRequestList, entity.getTransportMode(), consolidationId);
                updatedPackings = packingDao.updateEntityFromShipment(convertToEntityList(packingRequestList, Packing.class), id, deleteContainerIds);
                entity.setPackingList(updatedPackings);
            }
            List<UUID> deletedContGuids = new ArrayList<>();
            List<Packing> packsForSync = null;
            if(deleteContainerIds != null && deleteContainerIds.size() > 0) {
                ListCommonRequest listCommonRequest = constructListCommonRequest("containerId", deleteContainerIds, "IN");
                Pair<Specification<Packing>, Pageable> pair = fetchData(listCommonRequest, Packing.class);
                Page<Packing> packings = packingDao.findAll(pair.getLeft(), pair.getRight());
                if(packings != null && packings.getContent() != null && !packings.getContent().isEmpty()) {
                    List<Packing> packingList = new ArrayList<>();
                    for (Packing packing : packings.getContent()) {
                        packing.setContainerId(null);
                        packingList.add(packing);
                    }
                    packingDao.saveAll(packingList);
                    packsForSync = packingList;
                }
                listCommonRequest = constructListCommonRequest("id", deleteContainerIds, "IN");
                Pair<Specification<Containers>, Pageable> pair2 = fetchData(listCommonRequest, Containers.class);
                Page<Containers> containersPage = containerDao.findAll(pair2.getLeft(), pair2.getRight());
                if(containersPage != null && !containersPage.isEmpty())
                    deletedContGuids = containersPage.stream().map(e -> e.getGuid()).collect(Collectors.toList());
            }
            if (elDetailsRequestList != null) {
                List<ELDetails> updatedELDetails = elDetailsDao.updateEntityFromShipment(convertToEntityList(elDetailsRequestList, ELDetails.class), id);
                entity.setElDetailsList(updatedELDetails);
            }
            if (eventsRequestList != null) {
                List<Events> updatedEvents = eventDao.updateEntityFromOtherEntity(convertToEntityList(eventsRequestList, Events.class), id, Constants.SHIPMENT);
                entity.setEventsList(updatedEvents);
            }
            // Create events on basis of shipment status Confirmed/Created
            autoGenerateEvents(entity, previousStatus);
            if (jobRequestList != null) {
                List<Jobs> updatedJobs = jobDao.updateEntityFromShipment(convertToEntityList(jobRequestList, Jobs.class), id);
                entity.setJobsList(updatedJobs);
            }
            if (referenceNumbersRequestList != null) {
                List<ReferenceNumbers> updatedReferenceNumbers = referenceNumbersDao.updateEntityFromShipment(convertToEntityList(referenceNumbersRequestList, ReferenceNumbers.class), id);
                entity.setReferenceNumbersList(updatedReferenceNumbers);
            }
            if (routingsRequestList != null) {
                List<Routings> updatedRoutings = routingsDao.updateEntityFromShipment(convertToEntityList(routingsRequestList, Routings.class), id);
                entity.setRoutingsList(updatedRoutings);
            }
            if (serviceDetailsRequestList != null) {
                List<ServiceDetails> updatedServiceDetails = serviceDetailsDao.updateEntityFromShipment(convertToEntityList(serviceDetailsRequestList, ServiceDetails.class), id);
                entity.setServicesList(updatedServiceDetails);
            }
            if (fileRepoRequestList != null) {
                List<FileRepo> updatedFileRepos = fileRepoDao.updateEntityFromOtherEntity(convertToEntityList(fileRepoRequestList, FileRepo.class), id, Constants.SHIPMENT);
                entity.setFileRepoList(updatedFileRepos);
            }
            if (notesRequestList != null) {
                List<Notes> updatedNotes = notesDao.updateEntityFromOtherEntity(convertToEntityList(notesRequestList, Notes.class), id, Constants.SHIPMENT);
                entity.setNotesList(updatedNotes);
            }

            if (shipmentAddressList != null) {
                List<Parties> updatedParties = partiesDao.updateEntityFromOtherEntity(convertToEntityList(shipmentAddressList, Parties.class), id, Constants.SHIPMENT_ADDRESSES);
                entity.setShipmentAddresses(updatedParties);
            }

            // Create Shipment Route in Console for Auto Attach Consolidation;
            if (shipmentRequest.getReplaceConsoleRoute() != null && shipmentRequest.getReplaceConsoleRoute()){
                createShipmentRouteInConsole(shipmentRequest);
            }

            Hbl hbl = null;
            ConsolidationDetails consolidationDetails = null;
            if(updatedContainers.size() > 0) {
                hbl = hblService.checkAllContainerAssigned(id, updatedContainers, updatedPackings);
                if((tempConsolIds == null || tempConsolIds.size() == 0) && (shipmentSettingsDetails.getIsShipmentLevelContainer() == null || !shipmentSettingsDetails.getIsShipmentLevelContainer())) {
                    consolidationDetails = createConsolidation(entity, updatedContainers);
                }
            }
            afterSave(entity, false);
            updateMasterBill(entity, oldEntity.get().getMasterBill());
            updateLinkedShipmentData(entity, oldEntity.get());
            try {
                shipmentSync.sync(entity, deletedContGuids);
            } catch (Exception e){
                log.error("Error performing sync on shipment entity, {}", e);
            }
            if(hbl != null) {
                try {
                    hblSync.sync(hbl);
                }
                catch (Exception e) {
                    log.error("Error performing sync on hbl entity, {}", e);
                }
            }
            if(consolidationDetails != null) {
                try {
                    consolidationSync.sync(consolidationDetails);
                } catch (Exception e) {
                    log.error("Error performing sync on consol entity, {}", e);
                }
            }
            if(packsForSync != null) {
                try {
                    packingsSync.sync(packsForSync);
                } catch (Exception e) {
                    log.error("Error performing sync on packings list, {}", e);
                }
            }
            ShipmentDetailsResponse response = shipmentDetailsMapper.map(entity);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            throw new RuntimeException(e.getMessage());
        }
    }

    private void beforeSave(ShipmentDetails shipmentDetails) {
        if(!IsStringNullOrEmpty(shipmentDetails.getJobType()) && shipmentDetails.getJobType().equals(Constants.SHIPMENT_TYPE_DRT)){
            shipmentDetails.setHouseBill(shipmentDetails.getMasterBill());
        }
        v1ServiceUtil.validateCreditLimit(shipmentDetails.getClient(), ShipmentConstants.SHIPMENT_CREATION, shipmentDetails.getGuid());
    }

    public void afterSave(ShipmentDetails shipmentDetails, boolean isCreate) {
        try {
            if(shipmentDetails.getTenantId() == null)
                shipmentDetails.setTenantId(TenantContext.getCurrentTenant());
            KafkaResponse kafkaResponse = producer.getKafkaResponse(shipmentDetails, isCreate);
            producer.produceToKafka(jsonHelper.convertToJson(kafkaResponse), senderQueue, UUID.randomUUID().toString());
        }
        catch (Exception e) {
            log.error("Error Producing shipment to kafka, error is due to " + e.getMessage());
        }
        try {
            if(shipmentDetails.getStatus() != null && !Objects.equals(shipmentDetails.getStatus(), ShipmentStatus.Completed.getValue()) || shipmentDetails.getStatus() != null && !Objects.equals(shipmentDetails.getStatus(), ShipmentStatus.Cancelled.getValue())) {
                if (trackingServiceAdapter.checkIfConsolAttached(shipmentDetails)|| (shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) && shipmentDetails.getShipmentType().equals(Constants.SHIPMENT_TYPE_DRT) && !Objects.isNull(shipmentDetails.getHouseBill()))) {
                    UniversalTrackingPayload _utPayload = trackingServiceAdapter.mapShipmentDataToTrackingServiceData(shipmentDetails);
                    List<UniversalTrackingPayload> trackingPayloads = new ArrayList<>();
                    if(_utPayload != null) {
                        trackingPayloads.add(_utPayload);
                        var jsonBody = jsonHelper.convertToJson(trackingPayloads);
                        trackingServiceAdapter.publishUpdatesToTrackingServiceQueue(jsonBody, false);
                    }
                }
            }
            if(shipmentDetails.getSource() != null && shipmentDetails.getSource().equals(Constants.API)) {
                var events = trackingServiceAdapter.getAllEvents(shipmentDetails,null);
                var universalEventsPayload = trackingServiceAdapter.mapEventDetailsForTracking(shipmentDetails.getBookingReference(),Constants.SHIPMENT, shipmentDetails.getShipmentId(), events);
                List<UniversalTrackingPayload.UniversalEventsPayload> trackingPayloads= new ArrayList<>();
                if(universalEventsPayload != null) {
                    trackingPayloads.add(universalEventsPayload);
                    var jsonBody = jsonHelper.convertToJson(trackingPayloads);
                    trackingServiceAdapter.publishUpdatesToTrackingServiceQueue(jsonBody,true);
                }
            }
        }
        catch (Exception e) {
            log.error(e.getMessage());
        }
    }

    private void createShipmentRouteInConsole (ShipmentRequest shipmentRequest) throws Exception{
        List<ConsolidationDetailsRequest> consoleRequest = shipmentRequest.getConsolidationList();
        List<Routings> createRoutes = new ArrayList<>();
        if(shipmentRequest.getCreateMainLegRoute() != null && shipmentRequest.getCreateMainLegRoute()){
            List<RoutingsRequest> routeRequestList = shipmentRequest.getRoutingsList().stream().sorted(Comparator.comparingLong(RoutingsRequest::getLeg)).collect(Collectors.toList());
            var routeRequest = routeRequestList.stream().filter(x -> x.getMode().equals(shipmentRequest.getTransportMode())).findFirst();
            if(routeRequest.isPresent()) {
                createRoutes.add(convertToClass(routeRequest.get(), Routings.class));
                createRoutes = createConsoleRoutePayload(createRoutes);
            }
        } else {
            createRoutes = convertToEntityList(shipmentRequest.getRoutingsList(), Routings.class);
            createRoutes = createConsoleRoutePayload(createRoutes);
        }
        if(consoleRequest != null && !consoleRequest.isEmpty() && createRoutes != null && !createRoutes.isEmpty()) {
            for (var console : consoleRequest) {
                routingsDao.updateEntityFromConsole(createRoutes, console.getId());
            }
        }
    }

    private List<Routings> createConsoleRoutePayload(List<Routings> routes){
        List<Routings> responseList = new ArrayList<>();
        for (var route : routes){
            Routings routings = new Routings();
            routings.setLeg(1L);
            routings.setPol(route.getPol());
            routings.setPod(route.getPod());
            routings.setMode(route.getMode());
            routings.setEta(route.getEta());
            routings.setEtd(route.getEtd());
            routings.setTransitDays(route.getTransitDays());
            routings.setAta(route.getAta());
            routings.setAtd(route.getAtd());
            routings.setVesselId(route.getVesselId());
            routings.setVesselName(route.getVesselName());
            routings.setVoyage(route.getVoyage());
            routings.setCarrier(route.getCarrier());
            routings.setFlightNumber(route.getFlightNumber());
            responseList.add(routings);
        }
        return responseList;
    }

    public ConsolidationDetails createConsolidation(ShipmentDetails shipmentDetails, List<Containers> containers) {
        List<ShipmentSettingsDetails> shipmentSettingsDetails = shipmentSettingsDao.getSettingsByTenantIds(List.of(TenantContext.getCurrentTenant()));
        ShipmentSettingsDetails shipmentSettings = null;
        if(shipmentSettingsDetails != null && shipmentSettingsDetails.size() > 0)
            shipmentSettings = shipmentSettingsDetails.get(0);
        else {
            log.error("Not able to create consolidation, Shipment Settings not available in current tenant");
            return null;
        }
        if(shipmentSettings.getShipConsolidationContainerEnabled()) {
            ConsolidationDetails consolidationDetails = new ConsolidationDetails();
            consolidationDetails.setConsolidationType(Constants.SHIPMENT_TYPE_DRT);
            consolidationDetails.setTransportMode(shipmentDetails.getTransportMode());
            if((shipmentSettings.getConsolidationLite() == null || !shipmentSettings.getConsolidationLite()) &&
                    (shipmentDetails.getCarrierDetails().getOriginPort() == null || shipmentDetails.getCarrierDetails().getOriginPort().isEmpty()
                    || shipmentDetails.getCarrierDetails().getDestinationPort() == null || shipmentDetails.getCarrierDetails().getDestinationPort().isEmpty())) {
                throw new ValidationException("Not able to create consolidation, before adding 'New Containers' , pleasenprovide ‘Origin’ and ‘Destination’ values.");
            }
            if(shipmentDetails.getCarrierDetails().getOriginPort() == shipmentDetails.getCarrierDetails().getDestinationPort()) {
                throw new ValidationException("‘Origin’ and ‘Destination’ can't be same");
            }
            consolidationDetails.setCarrierDetails(jsonHelper.convertValue(shipmentDetails.getCarrierDetails(), CarrierDetails.class));
            consolidationDetails.getCarrierDetails().setId(null);
            consolidationDetails.getCarrierDetails().setGuid(null);
            consolidationDetails.getCarrierDetails().setOrigin(consolidationDetails.getCarrierDetails().getOriginPort());
            consolidationDetails.getCarrierDetails().setDestination(consolidationDetails.getCarrierDetails().getDestinationPort());
            consolidationDetails.setShipmentType(shipmentDetails.getDirection());
            consolidationDetails.setContainerCategory(shipmentDetails.getShipmentType());
            consolidationDetails.setIsReceivingAgentFreeTextAddress(false);
            consolidationDetails.setIsSendingAgentFreeTextAddress(false);
            consolidationDetails.setIsInland(false);
            consolidationDetails.setSourceTenantId(TenantContext.getCurrentTenant().longValue());
            consolidationService.generateConsolidationNumber(consolidationDetails);
            // TODO- which one is CarrierBookingRef
            // TODO- default organizations Row -- setAgentOrganizationAndAddress() function in v1
//            if(consolidationDetails.getShipmentType() != null && !consolidationDetails.getShipmentType().isEmpty()
//            && consolidationDetails.getShipmentType().equals(Constants.IMP) || consolidationDetails.getShipmentType().equals(Constants.DIRECTION_EXP)) {
//                Parties defaultParty = new Parties(); // TODO- fetch from tenants row
//                if(true) { // TODO- add conditions on default Party
//                    if(consolidationDetails.getShipmentType().equals(Constants.DIRECTION_EXP)) {
//
//                    }
//                }
//            }
            consolidationDetails = consolidationDetailsDao.save(consolidationDetails, false);
            Long id = consolidationDetails.getId();
            if(containers != null && containers.size() > 0) {
                containers = containers.stream().map(e -> e.setConsolidationId(id)).collect(Collectors.toList());
                containers = containerDao.saveAll(containers);
            }
            consolidationDetails.setContainersList(containers);
            attachConsolidations(shipmentDetails.getId(), List.of(id));
            consolidationService.afterSave(consolidationDetails, true);
            return consolidationDetails;
        }
        return null;
    }

    @Override
    public void exportExcel(HttpServletResponse response, CommonRequestModel commonRequestModel) throws IOException, IllegalAccessException {
        String responseMsg;

        ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
        if (request == null) {
            log.error("Request is empty for Shipment list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        request.setIncludeTbls(Arrays.asList("additionalDetails", "client", "consigner", "consignee", "carrierDetails"));
        Pair<Specification<ShipmentDetails>, Pageable> tuple = fetchData(request, ShipmentDetails.class, tableNames);
        Page<ShipmentDetails> shipmentDetailsPage = shipmentDao.findAll(tuple.getLeft(), tuple.getRight());
        log.info("Shipment list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());

        Workbook workbook = new XSSFWorkbook();
        Sheet sheet = workbook.createSheet("ShipmentList");
        makeHeadersInSheet(sheet);

        //Filling the data
        List<IRunnerResponse> shipmentListResponseData = convertEntityListToDtoList(shipmentDetailsPage.getContent());

        for (int i = 0; i < shipmentListResponseData.size(); i++) {
            Row itemRow = sheet.createRow(i + 2);
            ShipmentListResponse shipment = (ShipmentListResponse) shipmentListResponseData.get(i);
            var shipmentBasicValues = parser.getAllAttributeValuesAsList(shipment);
            int offset = 0;
            for (int j = 0; j < shipmentBasicValues.size(); j++)
                itemRow.createCell(offset + j).setCellValue(shipmentBasicValues.get(j));
            offset += shipmentBasicValues.size();

            var shipmentClientValues = parser.getAllAttributeValuesAsListForParty(shipment.getClient());
            for (int j = 0; j < shipmentClientValues.size(); j++)
                itemRow.createCell(offset + j).setCellValue(shipmentClientValues.get(j));
            offset += shipmentClientValues.size();

            var shipmentConsigneeValues = parser.getAllAttributeValuesAsListForParty(shipment.getConsignee());
            for (int j = 0; j < shipmentConsigneeValues.size(); j++)
                itemRow.createCell(offset + j).setCellValue(shipmentConsigneeValues.get(j));
            offset += shipmentConsigneeValues.size();

            var shipmentConsignerValues = parser.getAllAttributeValuesAsListForParty(shipment.getConsignee());
            for (int j = 0; j < shipmentConsignerValues.size(); j++)
                itemRow.createCell(offset + j).setCellValue(shipmentConsignerValues.get(j));
            offset += shipmentConsignerValues.size();

            var carrierDetails = parser.getAllAttributeValuesAsListForCarrier(shipment.getCarrierDetails());
            for (int j = 0; j < carrierDetails.size(); j++)
                itemRow.createCell(offset + j).setCellValue(carrierDetails.get(j));
            offset += carrierDetails.size();

            var pickupDetails = parser.getAllAttributeValuesAsListForPDDetail(shipment.getPickupDetails());
            for (int j = 0; j < pickupDetails.size(); j++)
                itemRow.createCell(offset + j).setCellValue(pickupDetails.get(j));
            offset += pickupDetails.size();

            var deliveryDetails = parser.getAllAttributeValuesAsListForPDDetail(shipment.getDeliveryDetails());
            for (int j = 0; j < deliveryDetails.size(); j++)
                itemRow.createCell(offset + j).setCellValue(deliveryDetails.get(j));
            offset += deliveryDetails.size();
        }

        LocalDateTime currentTime = LocalDateTime.now();
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyyMMddHHmmss");
        String timestamp = currentTime.format(formatter);
        String filenameWithTimestamp = "Shipments_" + ".xlsx";

        response.setContentType("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet");
        response.setHeader("Content-Disposition", "attachment; filename=" + filenameWithTimestamp);

        try (OutputStream outputStream = response.getOutputStream()) {
            workbook.write(outputStream);
        }

    }

    private void makeHeadersInSheet(Sheet sheet) {
        Row preHeaderRow = sheet.createRow(0);
        Row headerRow = sheet.createRow(1);
        List<String> shipmentHeader = parser.getHeadersForShipment();
        for (int i = 0; i < shipmentHeader.size(); i++) {
            Cell cell = headerRow.createCell(i);
            cell.setCellValue(shipmentHeader.get(i));
        }

        //Create Parties Headers
        int offSet = shipmentHeader.size();

        preHeaderRow.createCell(offSet).setCellValue("Client");
        List<String> partiesClientHeader = parser.getHeadersForParties();
        for (int i = 0; i < partiesClientHeader.size(); i++) {
            Cell cell = headerRow.createCell(offSet + i);
            cell.setCellValue(partiesClientHeader.get(i));
        }
        sheet.addMergedRegion(new CellRangeAddress(0, 0, offSet, offSet + partiesClientHeader.size() - 1));
        offSet += partiesClientHeader.size();

        preHeaderRow.createCell(offSet).setCellValue("Consignee");
        List<String> partiesConsigneeHeader = parser.getHeadersForParties();
        for (int i = 0; i < partiesConsigneeHeader.size(); i++) {
            Cell cell = headerRow.createCell(offSet + i);
            cell.setCellValue(partiesConsigneeHeader.get(i));
        }
        sheet.addMergedRegion(new CellRangeAddress(0, 0, offSet, offSet + partiesConsigneeHeader.size() - 1));
        offSet += partiesConsigneeHeader.size();

        preHeaderRow.createCell(offSet).setCellValue("Consigner");
        List<String> partiesConsignerHeader = parser.getHeadersForParties();
        for (int i = 0; i < partiesConsignerHeader.size(); i++) {
            Cell cell = headerRow.createCell(offSet + i);
            cell.setCellValue(partiesConsignerHeader.get(i));
        }
        sheet.addMergedRegion(new CellRangeAddress(0, 0, offSet, offSet + partiesConsignerHeader.size() - 1));
        offSet += partiesConsignerHeader.size();

        preHeaderRow.createCell(offSet).setCellValue("Carrier Details");
        List<String> carrierHeader = parser.getHeadersForCarrier();
        for (int i = 0; i < carrierHeader.size(); i++) {
            Cell cell = headerRow.createCell(offSet + i);
            cell.setCellValue(carrierHeader.get(i));
        }
        sheet.addMergedRegion(new CellRangeAddress(0, 0, offSet, offSet + carrierHeader.size() - 1));
        offSet += carrierHeader.size();

        preHeaderRow.createCell(offSet).setCellValue("Pickup Details");
        List<String> pickupDeliveryDetails = parser.getHeadersForPDDetails();
        for (int i = 0; i < pickupDeliveryDetails.size(); i++) {
            Cell cell = headerRow.createCell(offSet + i);
            cell.setCellValue(pickupDeliveryDetails.get(i));
        }
        sheet.addMergedRegion(new CellRangeAddress(0, 0, offSet, offSet + pickupDeliveryDetails.size() - 1));
        offSet += pickupDeliveryDetails.size();

        preHeaderRow.createCell(offSet).setCellValue("Delivery Details");
        for (int i = 0; i < pickupDeliveryDetails.size(); i++) {
            Cell cell = headerRow.createCell(offSet + i);
            cell.setCellValue(pickupDeliveryDetails.get(i));
        }
        sheet.addMergedRegion(new CellRangeAddress(0, 0, offSet, offSet + pickupDeliveryDetails.size() - 1));
        offSet += pickupDeliveryDetails.size();

    }


    public ResponseEntity<?> fullShipmentsList(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            // TODO- implement actual logic with filters
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Shipment list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            request.setIncludeTbls(Arrays.asList("additionalDetails", "client", "consigner", "consignee", "carrierDetails"));
//            checkWayBillNumberCriteria(request);
            Pair<Specification<ShipmentDetails>, Pageable> tuple = fetchData(request, ShipmentDetails.class, tableNames);
            Page<ShipmentDetails> shipmentDetailsPage = shipmentDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Shipment list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildListSuccessResponse(convertEntityListToFullShipmentList(shipmentDetailsPage.getContent()));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<?> assignShipmentContainers(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ShipmentContainerAssignRequest request = (ShipmentContainerAssignRequest) commonRequestModel.getData();
            ShipmentSettingsDetails shipmentSettingsDetails = shipmentSettingsDao.getSettingsByTenantIds(List.of(TenantContext.getCurrentTenant())).get(0);
            ShipmentDetails shipmentDetails = shipmentDao.findById(request.getShipmentId()).get();
            if(shipmentSettingsDetails.getMultipleShipmentEnabled() && (shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA) || shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_ROA))) {
                ListCommonRequest listCommonRequest = constructListCommonRequest("id", request.getContainerIds(), "IN");
                Pair<Specification<Containers>, Pageable> pair = fetchData(listCommonRequest, Containers.class);
                Page<Containers> containers = containerDao.findAll(pair.getLeft(), pair.getRight());
                boolean isFCL = shipmentDetails.getShipmentType().equals(Constants.CARGO_TYPE_FCL) && (shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA) || shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_ROA));
                if(containers != null && containers.getContent() != null) {
                    List<Containers> containersList = containers.getContent();
                    if(!containers.getContent().isEmpty()) {
                        for (Containers container : containersList) {
                            boolean isPart = container.getIsPart() != null && container.getIsPart().booleanValue();
                            if((shipmentDetails.getShipmentType().equals(Constants.CARGO_TYPE_FCL) || !isPart) && container.getShipmentsList() != null && container.getShipmentsList().size() > 0) {
                                String errorMsg = "This container is already linked to another shipment. Only part Container/Containers are allowed to attach";
                                if(isPart)
                                    errorMsg = "Mentioned container " + container.getContainerNumber() + " is already assigned to a Shipment - " + container.getShipmentsList().get(0).getShipmentId() + ". Please check and retry.";
                                throw new ValidationException(errorMsg);
                            }
                            if(isFCL) {
                                container.setAchievedWeight(container.getAllocatedWeight());
                                container.setAchievedVolume(container.getAllocatedVolume());
                                container.setAchievedWeightUnit(container.getAllocatedWeightUnit());
                                container.setAchievedVolumeUnit(container.getAllocatedVolumeUnit());
                                container.setWeightUtilization("100");
                                container.setVolumeUtilization("100");
                            }
                        }
                    }
                    if(isFCL)
                        containerDao.saveAll(containersList);
                }
            }
            shipmentsContainersMappingDao.assignContainers(request.getShipmentId(), request.getContainerIds());
            return ResponseHelper.buildSuccessResponse();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<?> assignAllContainers(CommonRequestModel commonRequestModel) {
        String responseMsg;
        ShipmentSettingsDetails shipmentSettingsDetails = shipmentSettingsDao.getSettingsByTenantIds(List.of(TenantContext.getCurrentTenant())).get(0);
        boolean lclAndSeaOrRoadFlag = shipmentSettingsDetails.getMultipleShipmentEnabled() != null && shipmentSettingsDetails.getMultipleShipmentEnabled();
        boolean IsConsolidatorFlag = shipmentSettingsDetails.getIsConsolidator() != null && shipmentSettingsDetails.getIsConsolidator();
        List<Containers> containersList = new ArrayList<>();
        try {
            ContainerAssignListRequest containerAssignRequest = (ContainerAssignListRequest) commonRequestModel.getData();
            Long shipmentId = containerAssignRequest.getShipmentId();
            Long consolidationId = containerAssignRequest.getConsolidationId();
            if (lclAndSeaOrRoadFlag) {
                if(!containerAssignRequest.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA) && !containerAssignRequest.getTransportMode().equals(Constants.TRANSPORT_MODE_ROA)) {
                    lclAndSeaOrRoadFlag = false;
                }
            }
            ListCommonRequest listCommonRequest = constructListCommonRequest("consolidationId", consolidationId, "=");
            Pair<Specification<Containers>, Pageable> pair = fetchData(listCommonRequest, Containers.class);
            Page<Containers> containers = containerDao.findAll(pair.getLeft(), pair.getRight());
            List<Containers> conts = new ArrayList<>();
            if(lclAndSeaOrRoadFlag) {
                for (Containers container : containers.getContent()) {
                    List<ShipmentsContainersMapping> shipmentsContainersMappings = shipmentsContainersMappingDao.findByContainerId(container.getId());
                    if(!shipmentsContainersMappings.stream().map(ShipmentsContainersMapping::getShipmentId).toList().contains(shipmentId)) {

                        if(container.getAllocatedWeight() != null && container.getAchievedWeight() != null && container.getAllocatedVolume() != null && container.getAchievedWeight() != null
                                && isNotEmpty(container.getAllocatedWeightUnit()) && isNotEmpty(container.getAllocatedVolumeUnit()) && isNotEmpty(container.getAchievedWeightUnit()) && isNotEmpty(container.getAchievedVolumeUnit())) {

                            BigDecimal achievedWeight = new BigDecimal(convertUnit(Constants.MASS, container.getAchievedWeight(), container.getAchievedWeightUnit(), container.getAllocatedWeightUnit()).toString());
                            BigDecimal achievedVolume = new BigDecimal(convertUnit(Constants.VOLUME, container.getAchievedVolume(), container.getAchievedVolumeUnit(), container.getAllocatedVolumeUnit()).toString());

                            if(achievedWeight.compareTo(container.getAllocatedWeight()) < 0 && achievedVolume.compareTo(container.getAllocatedVolume()) < 0) {
                                containersList.add(container);
                            }
                            else if(!IsConsolidatorFlag) {
                                conts.add(container);
                            }
                        }
                        else
                            containersList.add(container);
                    }
                }
                if(conts.size() > 0) {
                    for (Containers x : conts) {
                        boolean flag = true;
                        if(x.getShipmentsList() != null && x.getShipmentsList().size() > 0) {
                            for(ShipmentDetails shipmentDetails : x.getShipmentsList()) {
                                if(shipmentDetails.getShipmentType().equals(Constants.CARGO_TYPE_FCL))
                                    flag = false;
                            }
                        }
                        if (flag)
                            containersList.add(x);
                    }
                }
            }
            else {
                for (Containers container : containers.getContent()) {
                    List<ShipmentsContainersMapping> shipmentsContainersMappings = shipmentsContainersMappingDao.findByContainerId(container.getId());
                    if(!shipmentsContainersMappings.stream().map(ShipmentsContainersMapping::getShipmentId).toList().contains(shipmentId)) {
                        containersList.add(container);
                    }
                }
            }

            List<Long> containerIds = new ArrayList<>();
            ShipmentDetails shipmentDetails = shipmentDao.findById(containerAssignRequest.getShipmentId()).get();
            if(shipmentSettingsDetails.getMultipleShipmentEnabled() && (shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA) || shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_ROA))) {
                boolean isFCL = shipmentDetails.getShipmentType().equals(Constants.CARGO_TYPE_FCL) && (shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA) || shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_ROA));
                for (Containers container : containersList) {
                    boolean isPart = container.getIsPart() != null && container.getIsPart().booleanValue();
                    if ((shipmentDetails.getShipmentType().equals(Constants.CARGO_TYPE_FCL) || isPart) && container.getShipmentsList() != null && container.getShipmentsList().size() > 0) {

                    }
                    else {
                        containerIds.add(container.getId());
                    }
                    if (isFCL) {
                        container.setAchievedWeight(container.getAllocatedWeight());
                        container.setAchievedVolume(container.getAllocatedVolume());
                        container.setAchievedWeightUnit(container.getAllocatedWeightUnit());
                        container.setAchievedVolumeUnit(container.getAllocatedVolumeUnit());
                        container.setWeightUtilization("100");
                        container.setVolumeUtilization("100");
                    }
                }
                if(isFCL)
                    containerDao.saveAll(containersList);
            }
            shipmentsContainersMappingDao.assignContainers(containerAssignRequest.getShipmentId(), containerIds);
            return ResponseHelper.buildSuccessResponse();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private List<IRunnerResponse> convertEntityListToFullShipmentList(List<ShipmentDetails> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(shipmentDetail -> {
            ShipmentDetailsResponse response = modelMapper.map(shipmentDetail, ShipmentDetailsResponse.class);
            // TODO- check if they want status
//            if (shipmentDetail.getStatus() != null && shipmentDetail.getStatus() < ShipmentStatus.values().length)
//                response.setShipmentStatus(ShipmentStatus.values()[shipmentDetail.getStatus()].toString());
            responseList.add(response);
        });
//        setLocationData(responseList);
//        setContainerTeu(lst, responseList);
//        setBillingData(lst, responseList);
        return responseList;
    }

    public ResponseEntity<?> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            // TODO- implement actual logic with filters
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Shipment list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            request.setIncludeTbls(Arrays.asList("additionalDetails", "client", "consigner", "consignee", "carrierDetails"));
            checkWayBillNumberCriteria(request);
            Pair<Specification<ShipmentDetails>, Pageable> tuple = fetchData(request, ShipmentDetails.class, tableNames);
            Page<ShipmentDetails> shipmentDetailsPage = shipmentDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Shipment list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            if(request.getIncludeColumns()==null||request.getIncludeColumns().size()==0)
            return ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(shipmentDetailsPage.getContent()),
                    shipmentDetailsPage.getTotalPages(),
                    shipmentDetailsPage.getTotalElements());
            else {
               List<IRunnerResponse>filtered_list=new ArrayList<>();
            for( var curr: convertEntityListToDtoList(shipmentDetailsPage.getContent())){
                RunnerPartialListResponse res=new RunnerPartialListResponse();
                res.setData(PartialFetchUtils.fetchPartialListData(curr,request.getIncludeColumns()));
                filtered_list.add( res);

            }
                return ResponseHelper.buildListSuccessResponse(
                        filtered_list,
                        shipmentDetailsPage.getTotalPages(),
                        shipmentDetailsPage.getTotalElements());
            }
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }

    }

    private void checkWayBillNumberCriteria(ListCommonRequest request)
    {
        if(request != null && request.getFilterCriteria() != null && request.getFilterCriteria().size() > 0)
        {
            checkForWayBillFilter(request.getFilterCriteria());
        }
    }

    private void checkForWayBillFilter(List<FilterCriteria> filterCriteriaList) {
        for(FilterCriteria filterCriteria: filterCriteriaList)
        {
            if(filterCriteria.getCriteria() != null && filterCriteria.getCriteria().getFieldName() != null &&
                    filterCriteria.getCriteria().getFieldName().equals("wayBillNumber") && filterCriteria.getCriteria().getValue() != null) {

                WayBillNumberFilterRequest wayBillNumberFilterRequest = new WayBillNumberFilterRequest();
                wayBillNumberFilterRequest.setWayBillNumber(filterCriteria.getCriteria().getValue().toString());
                GuidsListResponse guidsListResponse = v1Service.fetchWayBillNumberFilterGuids(wayBillNumberFilterRequest);
                filterCriteria.getCriteria().setFieldName("guid");
                filterCriteria.getCriteria().setOperator("IN");
                filterCriteria.getCriteria().setValue(guidsListResponse.getGuidsList());
            }
            if(filterCriteria.getInnerFilter() != null && filterCriteria.getInnerFilter().size() > 0) {
                checkForWayBillFilter(filterCriteria.getInnerFilter());
            }
        }
    }

    @Override
    @Async
    public CompletableFuture<ResponseEntity<?>> listAsync(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Shipment async list for Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            request.setIncludeTbls(Arrays.asList("additionalDetails", "client", "consigner", "consignee", "carrierDetails"));
            Pair<Specification<ShipmentDetails>, Pageable> tuple = fetchData(request, ShipmentDetails.class, tableNames);
            Page<ShipmentDetails> shipmentDetailsPage = shipmentDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Shipment async list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return CompletableFuture.completedFuture(ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(shipmentDetailsPage.getContent()),
                    shipmentDetailsPage.getTotalPages(),
                    shipmentDetailsPage.getTotalElements()));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return CompletableFuture.completedFuture(ResponseHelper.buildFailedResponse(responseMsg));
        }

    }

    public ResponseEntity<?> delete(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            // TODO- implement Validation logic
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.debug("Request is empty for Shipment delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.debug("Request Id is null for Shipment delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(id);
            if (!shipmentDetails.isPresent()) {
                log.debug("Shipment Details is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            String oldEntityJsonString = jsonHelper.convertToJson(shipmentDetails.get());
            shipmentDao.delete(shipmentDetails.get());

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(null)
                            .prevData(jsonHelper.readFromJson(oldEntityJsonString, ShipmentDetails.class))
                            .parent(ShipmentDetails.class.getSimpleName())
                            .parentId(shipmentDetails.get().getId())
                            .operation(DBOperationType.DELETE.name()).build()
            );

            log.info("Deleted Shipment details for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildSuccessResponse();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public ResponseEntity<?> retrieveById(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if(request.getId() == null && request.getGuid() == null) {
                log.error("Request Id and Guid are null for Shipment retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new RunnerException("Id and GUID can't be null. Please provide any one !");
            }
            Long id = request.getId();
            Optional<ShipmentDetails> shipmentDetails = Optional.ofNullable(null);
            if(id != null ){
                shipmentDetails = shipmentDao.findById(id);
            } else {
                UUID guid = UUID.fromString(request.getGuid());
                shipmentDetails = shipmentDao.findByGuid(guid);
            }
            if (!shipmentDetails.isPresent()) {
                log.debug("Shipment Details is null for the input with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("Shipment details fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            List<Notes> notes = notesDao.findByEntityIdAndEntityType(request.getId(), Constants.SHIPMENT_BOOKING);
            ShipmentDetailsResponse response = modelMapper.map(shipmentDetails.get(), ShipmentDetailsResponse.class);
            response.setCustomerBookingNotesList(convertToDtoList(notes,NotesResponse.class));
            createShipmentPayload(shipmentDetails.get(), response);
            //containerCountUpdate(shipmentDetails.get(), response);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Async
    public CompletableFuture<ResponseEntity<?>> retrieveByIdAsync(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Shipment async retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.error("Request Id is null for Shipment async retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(id);
            if (!shipmentDetails.isPresent()) {
                log.debug("Shipment Details is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            shipmentDetails.get().setFileRepoList(fileRepoDao.findByEntityIdAndEntityType(id, Constants.SHIPMENT));
            shipmentDetails.get().setNotesList(notesDao.findByEntityIdAndEntityType(id, Constants.SHIPMENT));
            log.info("Shipment details async fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            ShipmentDetailsResponse response = jsonHelper.convertValue(shipmentDetails.get(), ShipmentDetailsResponse.class);
            response.setCustomerBookingNotesList(convertToDtoList(notesDao.findByEntityIdAndEntityType(request.getId(), Constants.SHIPMENT_BOOKING),NotesResponse.class));
            //containerCountUpdate(shipmentDetails.get(), response);
            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(response));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return CompletableFuture.completedFuture(ResponseHelper.buildFailedResponse(responseMsg));
        }
    }

    public ResponseEntity<?> completeRetrieveById(CommonRequestModel commonRequestModel) throws ExecutionException, InterruptedException {
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Shipment complete retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.error("Request Id is null for Shipment complete retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();

            CommonRequestModel commonListRequestModel = CommonRequestModel.buildRequest(constructListCommonRequest("shipmentId", id, "="));
            CommonRequestModel commonListRequestModelbyEntityId = CommonRequestModel.buildRequest(constructListCommonRequest("entityId", id, "="));

            CompletableFuture<ResponseEntity<?>> shipmentsFuture = retrieveByIdAsync(commonRequestModel);
            RunnerResponse<ShipmentDetailsResponse> res = (RunnerResponse<ShipmentDetailsResponse>) shipmentsFuture.get().getBody();
            if(request.getIncludeColumns()==null||request.getIncludeColumns().size()==0)
                return ResponseHelper.buildSuccessResponse(res.getData());
            else
                return ResponseHelper.buildSuccessResponse(PartialFetchUtils.fetchPartialData(res, request.getIncludeColumns()));
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Transactional
    public ResponseEntity<?> partialUpdate(CommonRequestModel commonRequestModel) throws Exception {

        ShipmentPatchRequest shipmentRequest = (ShipmentPatchRequest) commonRequestModel.getData();
        if ((shipmentRequest.getId() == null && shipmentRequest.getGuid() == null) && (shipmentRequest.getShipmentId() == null || shipmentRequest.getShipmentId().get() == "")) {
            log.error("Request Id is null for update request with Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new Exception("Request Id is null");
        }
        List<BookingCarriageRequest> bookingCarriageRequestList = shipmentRequest.getBookingCarriagesList();
        List<PackingRequest> packingRequestList = shipmentRequest.getPackingList();
        AdditionalDetailRequest additionalDetailRequest = shipmentRequest.getAdditionalDetail();
        List<ContainerRequest> containerRequestList = shipmentRequest.getContainersList();
        List<ELDetailsRequest> elDetailsRequestList = shipmentRequest.getElDetailsList();
        List<EventsRequest> eventsRequestList = shipmentRequest.getEventsList();
        List<FileRepoRequest> fileRepoRequestList = shipmentRequest.getFileRepoList();
        List<JobRequest> jobRequestList = shipmentRequest.getJobsList();
        List<NotesRequest> notesRequestList = shipmentRequest.getNotesList();
        List<ReferenceNumbersRequest> referenceNumbersRequestList = shipmentRequest.getReferenceNumbersList();
        List<RoutingsRequest> routingsRequestList = shipmentRequest.getRoutingsList();
        List<ServiceDetailsRequest> serviceDetailsRequestList = shipmentRequest.getServicesList();
        CarrierPatchRequest carrierDetailRequest = shipmentRequest.getCarrierDetails();
        // TODO- implement Validation logic
        Long id = null;
        Optional<ShipmentDetails> oldEntity = null;
        ShipmentRequest fetchShipmentRequest = new ShipmentRequest();
        fetchShipmentRequest.setId(shipmentRequest.getId() != null ? shipmentRequest.getId().get() : null);
        fetchShipmentRequest.setGuid(shipmentRequest.getGuid());
        if(shipmentRequest.getId() != null || shipmentRequest.getGuid() != null) {
            oldEntity = retrieveByIdOrGuid(fetchShipmentRequest);
            id = oldEntity.get().getId();
        }
        else {
            ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentId", shipmentRequest.getShipmentId().get(), "=");
            Pair<Specification<ShipmentDetails>, Pageable> shipmentPair = fetchData(listCommonRequest, ShipmentDetails.class);
            Page<ShipmentDetails> shipmentDetails = shipmentDao.findAll(shipmentPair.getLeft(), shipmentPair.getRight());
            if(shipmentDetails != null && shipmentDetails.get().count() == 1) {
                oldEntity = shipmentDetails.get().findFirst();
                id = oldEntity.get().getId();
            }
            else if(shipmentDetails == null || shipmentDetails.get().count() == 0) {
                log.error("Shipment not available for update request with Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            else {
                log.error("More than one shipments available for update request with Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_INCORRECT_RESULT_SIZE_EXCEPTION_MSG);
            }
        }
        if (!oldEntity.isPresent()) {
            log.debug("Shipment Details is null for Id {}", shipmentRequest.getId());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        try {
            ShipmentDetails entity = oldEntity.get();
            Integer previousStatus = oldEntity.get().getStatus();
            shipmentDetailsMapper.update(shipmentRequest, entity);

            entity.setId(oldEntity.get().getId());
            List<Containers> updatedContainers = null;
            Long consolidationId = null;
            if(entity.getConsolidationList() != null && entity.getConsolidationList().size() > 0)
                consolidationId = entity.getConsolidationList().get(0).getId();
            if (containerRequestList != null) {
                updatedContainers = containerDao.updateEntityFromShipmentConsole(convertToEntityList(containerRequestList, Containers.class), consolidationId, id, false);
            } else {
                updatedContainers = oldEntity.get().getContainersList();
            }
            entity.setContainersList(updatedContainers);
            AdditionalDetails updatedAdditionalDetails = null;
            if (additionalDetailRequest != null) {
                updatedAdditionalDetails = additionalDetailDao.updateEntityFromShipment(convertToClass(additionalDetailRequest, AdditionalDetails.class));
                entity.setAdditionalDetails(updatedAdditionalDetails);
            }
            CarrierDetails updatedCarrierDetails = null;
            if (carrierDetailRequest != null) {
                updatedCarrierDetails = oldEntity.get().getCarrierDetails();
                carrierDetailsMapper.update(carrierDetailRequest, updatedCarrierDetails);
                entity.setCarrierDetails(oldEntity.get().getCarrierDetails());
            }
            beforeSave(entity);
            updateMasterBill(entity, oldEntity.get().getMasterBill());
            updateLinkedShipmentData(entity, oldEntity.get());

            entity = shipmentDao.update(entity, false);

            entity.setContainersList(updatedContainers);
            if (additionalDetailRequest != null) {
                entity.setAdditionalDetails(updatedAdditionalDetails);
            }
            if (carrierDetailRequest != null) {
                entity.setCarrierDetails(updatedCarrierDetails);
            }
            if (bookingCarriageRequestList != null) {
                List<BookingCarriage> updatedBookingCarriages = bookingCarriageDao.updateEntityFromShipment(convertToEntityList(bookingCarriageRequestList, BookingCarriage.class), id);
                entity.setBookingCarriagesList(updatedBookingCarriages);
            }
            if (packingRequestList != null) {
                List<Packing> updatedPackings = packingDao.updateEntityFromShipment(convertToEntityList(packingRequestList, Packing.class), id, null);
                entity.setPackingList(updatedPackings);
            }
            if (elDetailsRequestList != null) {
                List<ELDetails> updatedELDetails = elDetailsDao.updateEntityFromShipment(convertToEntityList(elDetailsRequestList, ELDetails.class), id);
                entity.setElDetailsList(updatedELDetails);
            }
            if (eventsRequestList != null) {
                List<Events> updatedEvents = eventDao.updateEntityFromOtherEntity(convertToEntityList(eventsRequestList, Events.class), id, Constants.SHIPMENT);
                entity.setEventsList(updatedEvents);
            }
            // Create events on basis of shipment status Confirmed/Created
            autoGenerateEvents(entity, previousStatus);
            if (fileRepoRequestList != null) {
                List<FileRepo> updatedFileRepos = fileRepoDao.updateEntityFromOtherEntity(convertToEntityList(fileRepoRequestList, FileRepo.class), id, Constants.SHIPMENT);
                entity.setFileRepoList(updatedFileRepos);
            }
            if (jobRequestList != null) {
                List<Jobs> updatedJobs = jobDao.updateEntityFromShipment(convertToEntityList(jobRequestList, Jobs.class), id);
                entity.setJobsList(updatedJobs);
            }
            if (notesRequestList != null) {
                List<Notes> updatedNotes = notesDao.updateEntityFromOtherEntity(convertToEntityList(notesRequestList, Notes.class), id, Constants.SHIPMENT);
                entity.setNotesList(updatedNotes);
            }
            if (referenceNumbersRequestList != null) {
                List<ReferenceNumbers> updatedReferenceNumbers = referenceNumbersDao.updateEntityFromShipment(convertToEntityList(referenceNumbersRequestList, ReferenceNumbers.class), id);
                entity.setReferenceNumbersList(updatedReferenceNumbers);
            }
            if (routingsRequestList != null) {
                List<Routings> updatedRoutings = routingsDao.updateEntityFromShipment(convertToEntityList(routingsRequestList, Routings.class), id);
                entity.setRoutingsList(updatedRoutings);
            }
            if (serviceDetailsRequestList != null) {
                List<ServiceDetails> updatedServiceDetails = serviceDetailsDao.updateEntityFromShipment(convertToEntityList(serviceDetailsRequestList, ServiceDetails.class), id);
                entity.setServicesList(updatedServiceDetails);
            }

            try {
                shipmentSync.sync(entity, null);
            } catch (Exception e) {
                log.error("Error performing sync on shipment entity, {}", e);
            }

            afterSave(entity, false);
            ShipmentDetailsResponse response = shipmentDetailsMapper.map(entity);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            throw new RunnerException(responseMsg);
        }
    }

    public ResponseEntity<?> toggleLock(CommonRequestModel commonRequestModel) {
        CommonGetRequest commonGetRequest = (CommonGetRequest) commonRequestModel.getData();
        Long id = commonGetRequest.getId();
        ShipmentDetails shipmentDetails = shipmentDao.findById(id).get();
        String lockingUser = shipmentDetails.getLockedBy();
        String currentUser = userContext.getUser().getUsername();

        if (shipmentDetails.getIsLocked() != null && shipmentDetails.getIsLocked()) {
            if (lockingUser != null && lockingUser.equals(currentUser))
                shipmentDetails.setIsLocked(false);
        } else {
            shipmentDetails.setIsLocked(true);
            shipmentDetails.setLockedBy(currentUser);
        }
        shipmentDetails = shipmentDao.save(shipmentDetails, false);
        shipmentSync.syncLockStatus(shipmentDetails);
        afterSave(shipmentDetails, false);
        return ResponseHelper.buildSuccessResponse();
    }


    private <T extends IRunnerResponse> List<T> getResponse(CompletableFuture<ResponseEntity<?>> responseEntity) throws ExecutionException, InterruptedException {
        RunnerListResponse runnerListResponse = (RunnerListResponse<T>) responseEntity.get().getBody();
        return (List<T>) runnerListResponse.getData();
    }

    private <T extends IRunnerResponse> List<T> getResponse(ResponseEntity<?> responseEntity) throws ExecutionException, InterruptedException {
        RunnerListResponse runnerListResponse = (RunnerListResponse<T>) responseEntity.getBody();
        return (List<T>) runnerListResponse.getData();
    }

    private <T extends IRunnerResponse> T getResponseEntity(ResponseEntity<?> responseEntity) throws ExecutionException, InterruptedException {
        RunnerResponse runnerResponse = (RunnerResponse<T>) responseEntity.getBody();
        return (T) runnerResponse.getData();
    }

    private String generateShipmentId() {
        List<ShipmentSettingsDetails> shipmentSettingsList = shipmentSettingsDao.list();
        String shipmentId = "";
        boolean flag = true;

        while(flag) {
            ListCommonRequest listRequest = constructListCommonRequest("shipmentId", shipmentId, "=");
            Pair<Specification<ShipmentDetails>, Pageable> pair = fetchData(listRequest, ShipmentDetails.class);
            Page<ShipmentDetails> shipments = shipmentDao.findAll(pair.getLeft(), pair.getRight());

            if(!shipmentId.isEmpty() && shipments.getTotalElements() == 0)
                flag = false;
            else {
                if(shipmentSettingsList != null && shipmentSettingsList.size() != 0 && shipmentSettingsList.get(0) != null && shipmentSettingsList.get(0).getCustomisedSequence()) {
                    try{
                        shipmentId = getCustomizedShipmentProcessNumber(shipmentSettingsList.get(0), ProductProcessTypes.ShipmentNumber);
                    } catch (Exception ignored) {
                        //
                        shipmentId = Constants.SHIPMENT_ID_PREFIX + getShipmentsSerialNumber();
                    }
                }
                if(StringUtility.isEmpty(shipmentId)) {
                    shipmentId = Constants.SHIPMENT_ID_PREFIX + getShipmentsSerialNumber();
                }
            }
        }
        return shipmentId;
//        if (shipmentSettingsList.isEmpty())
//            return StringUtility.getRandomString(10);
//        return createShipmentSequence(shipmentSettingsList.get(0));
    }

    private String getCustomizedShipmentProcessNumber(ShipmentSettingsDetails shipmentSettingsDetails, ProductProcessTypes productProcessType) {
        productEngine.populateEnabledTenantProducts(shipmentSettingsDetails);
        // to check the commmon sequence
        var sequenceNumber = productEngine.GetCommonSequenceNumber(currentShipment.getTransportMode(), ProductProcessTypes.Consol_Shipment_TI);
        if (sequenceNumber != null && !sequenceNumber.isEmpty()) {
            return sequenceNumber;
        }
        var identifiedProduct = productEngine.IdentifyProduct(currentShipment);
        if (identifiedProduct == null){
            return "";
        }
        var sequenceSettings = getNextNumberHelper.getProductSequence(identifiedProduct.getId(), productProcessType);
        if(sequenceSettings == null){
            sequenceSettings = productEngine.getShipmentProductWithOutContainerType(currentShipment, productProcessType);
            if (sequenceSettings == null)
            {
                // get default product type for shipment
                var defaultProduct = productEngine.getDefaultShipmentProduct();
                if (defaultProduct == null || identifiedProduct == defaultProduct) {
                    return "";
                }
                sequenceSettings = getNextNumberHelper.getProductSequence(defaultProduct.getId(), productProcessType);
                if (sequenceSettings == null) {
                    return "";
                }
            }
        }
        String prefix = sequenceSettings.getPrefix() == null ? "" : sequenceSettings.getPrefix();
        var user = UserContext.getUser();
        return getNextNumberHelper.generateCustomSequence(sequenceSettings, prefix, user.TenantId, true, null, false);
    }

    private String getShipmentsSerialNumber() {
        // Moving this responsibility to v1 sequnce table to avoid syncing overhead
        return v1Service.getShipmentSerialNumber();
//        SequenceIncrementor sequenceIncrementor = SequenceIncrementor.builder().entityId(1L).build();
//        sequenceIncrementorDao.save(sequenceIncrementor);
//        return sequenceIncrementor.getShipmentIncrementId().toString();
    }

    private String createShipmentSequence(ShipmentSettingsDetails shipmentSetting) {
        String sequence = generateSequence(shipmentSetting.getShipmentIdGenerationType(), shipmentSetting.getShipmentIdGenerationPrefix(), shipmentSetting.getShipmentIdGenerationCounter());
        if (shipmentSetting.getShipmentIdGenerationType() == GenerationType.Serial) {
            shipmentSetting.setShipmentIdGenerationCounter(shipmentSetting.getShipmentIdGenerationCounter() + 1);
            shipmentSettingsDao.save(shipmentSetting);
        }
        return sequence;
    }

    private String generateSequence(GenerationType generationType, String prefix, Integer counter) {
        if(generationType == null)
            return StringUtility.getRandomString(10);
        String suffix;
        switch (generationType) {
            case Random:
                suffix = StringUtility.getRandomString(10);
                break;
            case Serial:
                suffix = String.valueOf(counter);
                break;
            default:
                suffix = StringUtility.getEmptyString();
        }
        return !StringUtils.isEmpty(prefix) ? prefix + suffix : suffix;
    }

    @Transactional
    public ResponseEntity<?> completeV1ShipmentCreateAndUpdate(CommonRequestModel commonRequestModel, Map<UUID, String> map) throws Exception {

        ShipmentRequest shipmentRequest = (ShipmentRequest) commonRequestModel.getData();

        List<BookingCarriageRequest> bookingCarriageRequestList = shipmentRequest.getBookingCarriagesList();
        List<PackingRequest> packingRequestList = shipmentRequest.getPackingList();
        AdditionalDetailRequest additionalDetailRequest = shipmentRequest.getAdditionalDetails();
        List<ContainerRequest> containerRequestList = shipmentRequest.getContainersList();
        List<ELDetailsRequest> elDetailsRequestList = shipmentRequest.getElDetailsList();
        List<EventsRequest> eventsRequestList = shipmentRequest.getEventsList();
        List<FileRepoRequest> fileRepoRequestList = shipmentRequest.getFileRepoList();
        List<JobRequest> jobRequestList = shipmentRequest.getJobsList();
        List<NotesRequest> notesRequestList = shipmentRequest.getNotesList();
        List<ReferenceNumbersRequest> referenceNumbersRequestList = shipmentRequest.getReferenceNumbersList();
        List<RoutingsRequest> routingsRequestList = shipmentRequest.getRoutingsList();
        List<ServiceDetailsRequest> serviceDetailsRequestList = shipmentRequest.getServicesList();
        CarrierDetailRequest carrierDetailRequest = shipmentRequest.getCarrierDetails();

        // TODO- implement Validation logic
        UUID guid = shipmentRequest.getGuid();
        Optional<ShipmentDetails> oldEntity = shipmentDao.findByGuid(guid);

        List<ConsolidationDetails> tempConsolidations = new ArrayList<>();

        List<ConsolidationDetailsRequest> consolidationDetailsRequests = shipmentRequest.getConsolidationList();
        if(consolidationDetailsRequests != null && !consolidationDetailsRequests.isEmpty()) {
            for(ConsolidationDetailsRequest consolidation : consolidationDetailsRequests) {
                Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findByGuid(consolidation.getGuid());
                if(consolidationDetails.get() != null && consolidationDetails.get().getId() != null) {
                    tempConsolidations.add(consolidationDetails.get());
                }
            }
        }

        try {
            List<Containers> oldContainers = null;
            Long id = null;
            ShipmentDetails oldShipment = null;
            boolean isCreate = true;
            if(oldEntity != null && oldEntity.isPresent()) {
                oldShipment = oldEntity.get();
                id = oldEntity.get().getId();
                oldContainers = oldEntity.get().getContainersList();
                isCreate = false;
            }
            shipmentRequest.setConsolidationList(null);
            ShipmentDetails entity = objectMapper.convertValue(shipmentRequest, ShipmentDetails.class);
            if (!tempConsolidations.isEmpty())
                entity.setConsolidationList(tempConsolidations);
            entity.setId(id);
            List<Containers> updatedContainers = null;
            if (containerRequestList != null) {
                updatedContainers = containerDao.updateEntityFromShipmentV1(convertToEntityList(containerRequestList, Containers.class), oldContainers);
            } else if(oldEntity != null && !oldEntity.isEmpty()){
                updatedContainers = oldEntity.get().getContainersList();
            }
            entity.setContainersList(updatedContainers);

            if(id == null) {
                entity = shipmentDao.save(entity, true);
                id = entity.getId();
            } else {
                entity = shipmentDao.update(entity, true);
            }
//            Not needed, added consolidations while saving shipment
//            attachConsolidations(entity.getId(), tempConsolIds);

            if (bookingCarriageRequestList != null) {
                ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentId", entity.getId(), "=");
                Pair<Specification<BookingCarriage>, Pageable> bookingCarriagePair = fetchData(listCommonRequest, BookingCarriage.class);
                Page<BookingCarriage> oldBookingCarriages = bookingCarriageDao.findAll(bookingCarriagePair.getLeft(), bookingCarriagePair.getRight());
                List<BookingCarriage> updatedBookingCarriages = bookingCarriageDao.updateEntityFromShipment(convertToEntityList(bookingCarriageRequestList, BookingCarriage.class), id, oldBookingCarriages.stream().toList());
                entity.setBookingCarriagesList(updatedBookingCarriages);
            }
            if (packingRequestList != null) {
                ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentId", entity.getId(), "=");
                Pair<Specification<Packing>, Pageable> packingPair = fetchData(listCommonRequest, Packing.class);
                Page<Packing> oldPackings = packingDao.findAll(packingPair.getLeft(), packingPair.getRight());
                List<Packing> updatedPackings = packingDao.updateEntityFromShipment(convertToEntityList(packingRequestList, Packing.class), id, oldPackings.stream().toList(), updatedContainers, map);
                entity.setPackingList(updatedPackings);
            }
            if (elDetailsRequestList != null) {
                ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentId", entity.getId(), "=");
                Pair<Specification<ELDetails>, Pageable> elDetailsPair = fetchData(listCommonRequest, ELDetails.class);
                Page<ELDetails> oldELDetails = elDetailsDao.findAll(elDetailsPair.getLeft(), elDetailsPair.getRight());
                List<ELDetails> updatedELDetails = elDetailsDao.updateEntityFromShipment(convertToEntityList(elDetailsRequestList, ELDetails.class), id, oldELDetails.stream().toList());
                entity.setElDetailsList(updatedELDetails);
            }
            if (eventsRequestList != null) {
                ListCommonRequest listCommonRequest = constructListRequestFromEntityId(entity.getId(), Constants.SHIPMENT);
                Pair<Specification<Events>, Pageable> pair = fetchData(listCommonRequest, Events.class);
                Page<Events> oldEvents = eventDao.findAll(pair.getLeft(), pair.getRight());
                List<Events> updatedEvents = eventDao.updateEntityFromOtherEntity(convertToEntityList(eventsRequestList, Events.class), id, Constants.SHIPMENT, oldEvents.stream().toList());
                entity.setEventsList(updatedEvents);
            }
            if (jobRequestList != null) {
                ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentId", entity.getId(), "=");
                Pair<Specification<Jobs>, Pageable> pair = fetchData(listCommonRequest, Jobs.class);
                Page<Jobs> oldJobs = jobDao.findAll(pair.getLeft(), pair.getRight());
                List<Jobs> updatedJobs = jobDao.updateEntityFromShipment(convertToEntityList(jobRequestList, Jobs.class), id, oldJobs.stream().toList());
                entity.setJobsList(updatedJobs);
            }
            if (referenceNumbersRequestList != null) {
                ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentId", entity.getId(), "=");
                Pair<Specification<ReferenceNumbers>, Pageable> pair = fetchData(listCommonRequest, ReferenceNumbers.class);
                Page<ReferenceNumbers> oldReferenceNumbers = referenceNumbersDao.findAll(pair.getLeft(), pair.getRight());
                List<ReferenceNumbers> updatedReferenceNumbers = referenceNumbersDao.updateEntityFromShipment(convertToEntityList(referenceNumbersRequestList, ReferenceNumbers.class), id, oldReferenceNumbers.stream().toList());
                entity.setReferenceNumbersList(updatedReferenceNumbers);
            }
            if (routingsRequestList != null) {
                ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentId", entity.getId(), "=");
                Pair<Specification<Routings>, Pageable> pair = fetchData(listCommonRequest, Routings.class);
                Page<Routings> oldRoutings = routingsDao.findAll(pair.getLeft(), pair.getRight());
                List<Routings> updatedRoutings = routingsDao.updateEntityFromShipment(convertToEntityList(routingsRequestList, Routings.class), id, oldRoutings.stream().toList());
                entity.setRoutingsList(updatedRoutings);
            }
            if (serviceDetailsRequestList != null) {
                ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentId", entity.getId(), "=");
                Pair<Specification<ServiceDetails>, Pageable> pair = fetchData(listCommonRequest, ServiceDetails.class);
                Page<ServiceDetails> oldServiceDetails = serviceDetailsDao.findAll(pair.getLeft(), pair.getRight());
                List<ServiceDetails> updatedServiceDetails = serviceDetailsDao.updateEntityFromShipment(convertToEntityList(serviceDetailsRequestList, ServiceDetails.class), id, oldServiceDetails.stream().toList());
                entity.setServicesList(updatedServiceDetails);
            }
            if (fileRepoRequestList != null) {
                ListCommonRequest listCommonRequest = constructListRequestFromEntityId(entity.getId(), Constants.SHIPMENT);
                Pair<Specification<FileRepo>, Pageable> pair = fetchData(listCommonRequest, FileRepo.class);
                Page<FileRepo> oldFileRepoList = fileRepoDao.findAll(pair.getLeft(), pair.getRight());
                List<FileRepo> updatedFileRepos = fileRepoDao.updateEntityFromOtherEntity(convertToEntityList(fileRepoRequestList, FileRepo.class), id, Constants.SHIPMENT, oldFileRepoList.stream().toList());
                entity.setFileRepoList(updatedFileRepos);
            }
            if (notesRequestList != null) {
                ListCommonRequest listCommonRequest = constructListRequestFromEntityId(entity.getId(), Constants.SHIPMENT);
                Pair<Specification<Notes>, Pageable> pair = fetchData(listCommonRequest, Notes.class);
                Page<Notes> oldNoteList = notesDao.findAll(pair.getLeft(), pair.getRight());
                List<Notes> updatedNotes = notesDao.updateEntityFromOtherEntity(convertToEntityList(notesRequestList, Notes.class), id, Constants.SHIPMENT, oldNoteList.stream().toList());
                entity.setNotesList(updatedNotes);
            }
            afterSave(entity, isCreate);
            ShipmentDetailsResponse response = shipmentDetailsMapper.map(entity);

            return ResponseHelper.buildSuccessResponse(response);
        } catch (ExecutionException e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }
    
    public ResponseEntity<?> calculateContainerSummary(CommonRequestModel commonRequestModel) throws Exception {
        String responseMsg;
        CalculateContainerSummaryRequest request = (CalculateContainerSummaryRequest) commonRequestModel.getData();
        try {
            List<Containers> containers = jsonHelper.convertValueToList(request.getContainersList(), Containers.class);
            ContainerSummaryResponse response = containerService.calculateContainerSummary(containers, request.getTransportMode(), request.getShipmentType());
            return ResponseHelper.buildSuccessResponse(response);
        }
        catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public ResponseEntity<?> calculatePackSummary(CommonRequestModel commonRequestModel) throws Exception {
        String responseMsg;
        CalculatePackSummaryRequest request = (CalculatePackSummaryRequest) commonRequestModel.getData();
        try {
            List<Packing> packingList = jsonHelper.convertValueToList(request.getPackingList(), Packing.class);
            PackSummaryResponse response = packingService.calculatePackSummary(packingList, request.getTransportMode(), request.getShipmentType(), new ShipmentMeasurementDetailsDto());
            return ResponseHelper.buildSuccessResponse(response);
        }
        catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<?> getAllMasterData(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            Long id = commonRequestModel.getId();
            Optional<ShipmentDetails> shipmentDetailsOptional = shipmentDao.findById(id);
            if(shipmentDetailsOptional == null || !shipmentDetailsOptional.isPresent()) {
                log.debug("Shipment Details is null for Id {}", id);
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            ShipmentDetails shipmentDetails = shipmentDetailsOptional.get();
            ShipmentDetailsResponse shipmentDetailsResponse = jsonHelper.convertValue(shipmentDetails, ShipmentDetailsResponse.class);
            Map<String, Object> response = fetchAllMasterDataByKey(shipmentDetails, shipmentDetailsResponse);
            return ResponseHelper.buildSuccessResponse(response);
        }
        catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_DATA_RETRIEVAL_FAILURE;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public Map<String, Object> fetchAllMasterDataByKey(ShipmentDetails shipmentDetails, ShipmentDetailsResponse shipmentDetailsResponse) {
        Map<String, Object> masterDataResponse = new HashMap<>();
        var masterListFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllMasterDataInSingleCall(shipmentDetails, shipmentDetailsResponse, masterDataResponse)), executorService);
        var unLocationsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllUnlocationDataInSingleCall(shipmentDetails, shipmentDetailsResponse, masterDataResponse)), executorService);
        var carrierFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllCarrierDataInSingleCall(shipmentDetails, shipmentDetailsResponse, masterDataResponse)), executorService);
        var currencyFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllCurrencyDataInSingleCall(shipmentDetails, shipmentDetailsResponse, masterDataResponse)), executorService);
        var commodityTypesFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllCommodityTypesInSingleCall(shipmentDetails, shipmentDetailsResponse, masterDataResponse)), executorService);
        var tenantDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllTenantDataInSingleCall(shipmentDetails, shipmentDetailsResponse, masterDataResponse)), executorService);
        var wareHouseDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllWarehouseDataInSingleCall(shipmentDetails, shipmentDetailsResponse, masterDataResponse)), executorService);
        var activityDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllActivityDataInSingleCall(shipmentDetails, shipmentDetailsResponse, masterDataResponse)), executorService);
        var salesAgentFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllSalesAgentInSingleCall(shipmentDetails, shipmentDetailsResponse, masterDataResponse)), executorService);
        var containerTypeFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllContainerTypesInSingleCall(shipmentDetails, shipmentDetailsResponse, masterDataResponse)), executorService);
        var vesselsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllVesselDataInSingleCall(shipmentDetails, shipmentDetailsResponse, masterDataResponse)), executorService);
        var dgSubstanceFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllDGSubstanceDataInSingleCall(shipmentDetails, shipmentDetailsResponse, masterDataResponse)), executorService);
        CompletableFuture.allOf(masterListFuture, unLocationsFuture, carrierFuture, currencyFuture, commodityTypesFuture, tenantDataFuture, wareHouseDataFuture, activityDataFuture, salesAgentFuture,
                containerTypeFuture, vesselsFuture, dgSubstanceFuture).join();

        return masterDataResponse;
    }

    public void createShipmentPayload(ShipmentDetails shipmentDetails, ShipmentDetailsResponse shipmentDetailsResponse) {
        try {
            var masterListFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllMasterDataInSingleCall(shipmentDetails, shipmentDetailsResponse, null)), executorService);
            var unLocationsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllUnlocationDataInSingleCall(shipmentDetails, shipmentDetailsResponse, null)), executorService);
            var carrierFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllCarrierDataInSingleCall(shipmentDetails, shipmentDetailsResponse, null)), executorService);
            var currencyFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllCurrencyDataInSingleCall(shipmentDetails, shipmentDetailsResponse, null)), executorService);
            var commodityTypesFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllCommodityTypesInSingleCall(shipmentDetails, shipmentDetailsResponse, null)), executorService);
            var tenantDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllTenantDataInSingleCall(shipmentDetails, shipmentDetailsResponse, null)), executorService);
            var wareHouseDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllWarehouseDataInSingleCall(shipmentDetails, shipmentDetailsResponse, null)), executorService);
            var activityDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllActivityDataInSingleCall(shipmentDetails, shipmentDetailsResponse, null)), executorService);
            var salesAgentFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllSalesAgentInSingleCall(shipmentDetails, shipmentDetailsResponse, null)), executorService);
            var containerTypeFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllContainerTypesInSingleCall(shipmentDetails, shipmentDetailsResponse, null)), executorService);
            CompletableFuture.allOf(masterListFuture, unLocationsFuture, carrierFuture, currencyFuture, commodityTypesFuture, tenantDataFuture, wareHouseDataFuture, activityDataFuture, salesAgentFuture,
                    containerTypeFuture).join();
            setContainersPacksAutoUpdateData(shipmentDetailsResponse);
            shipmentDetailsResponse.setPackSummary(packingService.calculatePackSummary(shipmentDetails.getPackingList(), shipmentDetails.getTransportMode(), shipmentDetails.getShipmentType(), new ShipmentMeasurementDetailsDto()));
            shipmentDetailsResponse.setContainerSummary(containerService.calculateContainerSummary(shipmentDetails.getContainersList(), shipmentDetails.getTransportMode(), shipmentDetails.getShipmentType()));
        }
        catch (Exception ex) {
            log.error("Request: {} || Error occured for event: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), IntegrationType.MASTER_DATA_FETCH_FOR_SHIPMENT_RETRIEVE, ex.getLocalizedMessage());
        }

    }

    private CompletableFuture<ResponseEntity<?>> addAllMasterDataInSingleCall (ShipmentDetails shipmentDetails, ShipmentDetailsResponse shipmentDetailsResponse, Map<String, Object> masterDataResponse) {

        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        List<MasterListRequest> listRequests = new ArrayList<>(masterDataUtils.createInBulkMasterListRequest(shipmentDetailsResponse, ShipmentDetails.class, fieldNameKeyMap, ShipmentDetails.class.getSimpleName() ));
        if (!Objects.isNull(shipmentDetailsResponse.getAdditionalDetails()))
            listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(shipmentDetailsResponse.getAdditionalDetails(), AdditionalDetails.class, fieldNameKeyMap, AdditionalDetails.class.getSimpleName() ));
        if (!Objects.isNull(shipmentDetailsResponse.getCarrierDetails()))
            listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(shipmentDetailsResponse.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName() ));

        if(masterDataResponse != null) {
            if(!Objects.isNull(shipmentDetailsResponse.getRoutingsList()))
                shipmentDetailsResponse.getRoutingsList().forEach(r -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(r, Routings.class, fieldNameKeyMap, Routings.class.getSimpleName() + r.getId())));
            if(!Objects.isNull(shipmentDetailsResponse.getBookingCarriagesList()))
                shipmentDetailsResponse.getBookingCarriagesList().forEach(r -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(r, BookingCarriage.class, fieldNameKeyMap, BookingCarriage.class.getSimpleName() + r.getId())));
            if(!Objects.isNull(shipmentDetailsResponse.getPackingList()))
                shipmentDetailsResponse.getPackingList().forEach(r -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(r, Packing.class, fieldNameKeyMap, Packing.class.getSimpleName() + r.getId())));
            if(!Objects.isNull(shipmentDetailsResponse.getReferenceNumbersList()))
                shipmentDetailsResponse.getReferenceNumbersList().forEach(r -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(r, ReferenceNumbers.class, fieldNameKeyMap, ReferenceNumbers.class.getSimpleName() + r.getId())));
            if(!Objects.isNull(shipmentDetailsResponse.getServicesList()))
                shipmentDetailsResponse.getServicesList().forEach(r -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(r, ServiceDetails.class, fieldNameKeyMap, ServiceDetails.class.getSimpleName() + r.getId())));
        }

        MasterListRequestV2 masterListRequestV2 = new MasterListRequestV2();
        masterListRequestV2.setMasterListRequests(listRequests);
        masterListRequestV2.setIncludeCols(Arrays.asList("ItemType", "ItemValue", "ItemDescription", "ValuenDesc", "Cascade"));

        Map<String, EntityTransferMasterLists> keyMasterDataMap = masterDataUtils.fetchInBulkMasterList(masterListRequestV2);
        masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.MASTER_LIST);

        if(masterDataResponse == null) {
            shipmentDetailsResponse.setMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(ShipmentDetails.class.getSimpleName()), CacheConstants.MASTER_LIST));
            if (!Objects.isNull(shipmentDetailsResponse.getAdditionalDetails()))
                shipmentDetailsResponse.getAdditionalDetails().setMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(AdditionalDetails.class.getSimpleName()), CacheConstants.MASTER_LIST) );
            if (!Objects.isNull(shipmentDetailsResponse.getCarrierDetails()))
                shipmentDetailsResponse.getCarrierDetails().setMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName()), CacheConstants.MASTER_LIST) );
        }
        else {
            masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.MASTER_LIST, masterDataResponse);
        }

        return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(keyMasterDataMap));
    }

    private CompletableFuture<ResponseEntity<?>> addAllUnlocationDataInSingleCall (ShipmentDetails shipmentDetails, ShipmentDetailsResponse shipmentDetailsResponse, Map<String, Object> masterDataResponse) {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        List<String> locationCodes = new ArrayList<>();
        if (!Objects.isNull(shipmentDetailsResponse.getCarrierDetails()))
            locationCodes.addAll((masterDataUtils.createInBulkUnLocationsRequest(shipmentDetailsResponse.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName() )));
        if (!Objects.isNull(shipmentDetailsResponse.getAdditionalDetails()))
            locationCodes.addAll((masterDataUtils.createInBulkUnLocationsRequest(shipmentDetailsResponse.getAdditionalDetails(), AdditionalDetails.class, fieldNameKeyMap, AdditionalDetails.class.getSimpleName() )));

        if(masterDataResponse != null) {
            if(!Objects.isNull(shipmentDetailsResponse.getRoutingsList()))
                shipmentDetailsResponse.getRoutingsList().forEach(r -> locationCodes.addAll(masterDataUtils.createInBulkUnLocationsRequest(r, Routings.class, fieldNameKeyMap, Routings.class.getSimpleName() + r.getId())));
            if(!Objects.isNull(shipmentDetailsResponse.getBookingCarriagesList()))
                shipmentDetailsResponse.getBookingCarriagesList().forEach(r -> locationCodes.addAll(masterDataUtils.createInBulkUnLocationsRequest(r, BookingCarriage.class, fieldNameKeyMap, BookingCarriage.class.getSimpleName() + r.getId())));
            if(!Objects.isNull(shipmentDetailsResponse.getContainersList()))
                shipmentDetailsResponse.getContainersList().forEach(r -> locationCodes.addAll(masterDataUtils.createInBulkUnLocationsRequest(r, Containers.class, fieldNameKeyMap, Containers.class.getSimpleName() + r.getId())));
            if(!Objects.isNull(shipmentDetailsResponse.getServicesList()))
                shipmentDetailsResponse.getServicesList().forEach(r -> locationCodes.addAll(masterDataUtils.createInBulkUnLocationsRequest(r, ServiceDetails.class, fieldNameKeyMap, ServiceDetails.class.getSimpleName() + r.getId())));
        }

        Map<String, EntityTransferUnLocations> keyMasterDataMap = masterDataUtils.fetchInBulkUnlocations(locationCodes, EntityTransferConstants.LOCATION_SERVICE_GUID);
        masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.UNLOCATIONS);

        if(masterDataResponse == null) {
            if (!Objects.isNull(shipmentDetailsResponse.getCarrierDetails()))
                shipmentDetailsResponse.getCarrierDetails().setUnlocationData(masterDataUtils.setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName()), CacheConstants.UNLOCATIONS));
            if (!Objects.isNull(shipmentDetailsResponse.getAdditionalDetails()))
                shipmentDetailsResponse.getAdditionalDetails().setUnlocationData(masterDataUtils.setMasterData(fieldNameKeyMap.get(AdditionalDetails.class.getSimpleName()), CacheConstants.UNLOCATIONS));
        }
        else {
            masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.UNLOCATIONS, masterDataResponse);
        }

        return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(keyMasterDataMap));
    }

    private CompletableFuture<ResponseEntity<?>> addAllTenantDataInSingleCall (ShipmentDetails shipmentDetails, ShipmentDetailsResponse shipmentDetailsResponse, Map<String, Object> masterDataResponse) {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        List<String> tenantIdList = new ArrayList<>(masterDataUtils.createInBulkTenantsRequest(shipmentDetailsResponse, ShipmentDetails.class, fieldNameKeyMap, ShipmentDetails.class.getSimpleName()));
        if(!Objects.isNull(shipmentDetailsResponse.getAdditionalDetails()))
            tenantIdList.addAll(masterDataUtils.createInBulkTenantsRequest(shipmentDetailsResponse.getAdditionalDetails(), AdditionalDetails.class, fieldNameKeyMap, AdditionalDetails.class.getSimpleName()));

        Map v1Data = masterDataUtils.fetchInTenantsList(tenantIdList);
        masterDataUtils.pushToCache(v1Data, CacheConstants.TENANTS);

        if(masterDataResponse == null) {
            shipmentDetailsResponse.setTenantIdsData(masterDataUtils.setMasterData(fieldNameKeyMap.get(ShipmentDetails.class.getSimpleName()), CacheConstants.TENANTS));
            if(!Objects.isNull(shipmentDetailsResponse.getAdditionalDetails()))
                shipmentDetailsResponse.getAdditionalDetails().setTenantIdsData(masterDataUtils.setMasterData(fieldNameKeyMap.get(AdditionalDetails.class.getSimpleName()), CacheConstants.TENANTS));
        }
        else {
            masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.TENANTS, masterDataResponse);
        }

        return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(v1Data));
    }

    private CompletableFuture<ResponseEntity<?>> addAllCurrencyDataInSingleCall (ShipmentDetails shipmentDetails, ShipmentDetailsResponse shipmentDetailsResponse, Map<String, Object> masterDataResponse) {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        List<String> currencyList = new ArrayList<>(masterDataUtils.createInBulkCurrencyRequest(shipmentDetailsResponse, ShipmentDetails.class, fieldNameKeyMap, ShipmentDetails.class.getSimpleName()));
        Map v1Data = masterDataUtils.fetchInCurrencyList(currencyList);
        masterDataUtils.pushToCache(v1Data, CacheConstants.CURRENCIES);

        if(masterDataResponse == null) {
            shipmentDetailsResponse.setCurrenciesMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(ShipmentDetails.class.getSimpleName()), CacheConstants.CURRENCIES));
        }
        else {
            masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.CURRENCIES, masterDataResponse);
        }

        return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(v1Data));
    }

    private CompletableFuture<ResponseEntity<?>> addAllCarrierDataInSingleCall (ShipmentDetails shipmentDetails, ShipmentDetailsResponse shipmentDetailsResponse, Map<String, Object> masterDataResponse) {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        List<String> carrierList = new ArrayList<>();
        if (!Objects.isNull(shipmentDetailsResponse.getCarrierDetails()))
            carrierList = new ArrayList<>(masterDataUtils.createInBulkCarriersRequest(shipmentDetailsResponse.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName()));

        if(masterDataResponse != null) {
            if (!Objects.isNull(shipmentDetailsResponse.getRoutingsList())) {
                List<String> finalCarrierList = carrierList;
                shipmentDetailsResponse.getRoutingsList().forEach(r -> finalCarrierList.addAll(masterDataUtils.createInBulkCarriersRequest(r, Routings.class, fieldNameKeyMap, Routings.class.getSimpleName() + r.getId() )));
            }
        }

        Map v1Data = masterDataUtils.fetchInBulkCarriers(carrierList);
        masterDataUtils.pushToCache(v1Data, CacheConstants.CARRIER);

        if(masterDataResponse == null) {
            shipmentDetailsResponse.getCarrierDetails().setCarrierMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName()), CacheConstants.CARRIER));
        }
        else {
            masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.CARRIER, masterDataResponse);
        }

        return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(v1Data));
    }

    private CompletableFuture<ResponseEntity<?>> addAllCommodityTypesInSingleCall(ShipmentDetails shipmentDetails, ShipmentDetailsResponse shipmentDetailsResponse, Map<String, Object> masterDataResponse) {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        Set<String> commodityTypes = new HashSet<>();
        if (!Objects.isNull(shipmentDetailsResponse.getContainersList()))
            shipmentDetailsResponse.getContainersList().forEach(r -> commodityTypes.addAll(masterDataUtils.createInBulkCommodityTypeRequest(r, Containers.class, fieldNameKeyMap, Containers.class.getSimpleName() + r.getId() )));

        if(masterDataResponse != null) {
            if (!Objects.isNull(shipmentDetailsResponse.getPackingList()))
                shipmentDetailsResponse.getPackingList().forEach(r -> commodityTypes.addAll(masterDataUtils.createInBulkCommodityTypeRequest(r, Packing.class, fieldNameKeyMap, Packing.class.getSimpleName() + r.getId() )));
        }

        Map<String, EntityTransferCommodityType> v1Data = masterDataUtils.fetchInBulkCommodityTypes(commodityTypes.stream().toList());
        masterDataUtils.pushToCache(v1Data, CacheConstants.COMMODITY);

        if(masterDataResponse == null) {
            if (!Objects.isNull(shipmentDetailsResponse.getContainersList()))
                shipmentDetailsResponse.getContainersList().forEach(r -> r.setCommodityTypeData(masterDataUtils.setMasterData(fieldNameKeyMap.get(Containers.class.getSimpleName() + r.getId()), CacheConstants.COMMODITY)));
        }
        else {
            masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.COMMODITY, masterDataResponse);
        }

        return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(v1Data));
    }

    private CompletableFuture<ResponseEntity<?>> addAllWarehouseDataInSingleCall (ShipmentDetails shipmentDetails, ShipmentDetailsResponse shipmentDetailsResponse, Map<String, Object> masterDataResponse) {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        Set<String> wareHouseTypes = new HashSet<>();
        if (!Objects.isNull(shipmentDetailsResponse.getAdditionalDetails()))
            wareHouseTypes.addAll(masterDataUtils.createInBulkWareHouseRequest(shipmentDetailsResponse.getAdditionalDetails(), AdditionalDetails.class, fieldNameKeyMap, AdditionalDetails.class.getSimpleName()) );

        Map v1Data = masterDataUtils.fetchInWareHousesList(wareHouseTypes.stream().toList());
        masterDataUtils.pushToCache(v1Data, CacheConstants.WAREHOUSES);

        if(masterDataResponse == null) {
            if (!Objects.isNull(shipmentDetailsResponse.getAdditionalDetails()))
                shipmentDetailsResponse.getAdditionalDetails().addTextData(masterDataUtils.setMasterData(fieldNameKeyMap.get(AdditionalDetails.class.getSimpleName()), CacheConstants.WAREHOUSES));
        }
        else {
            masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.WAREHOUSES, masterDataResponse);
        }

        return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(v1Data));
    }

    private CompletableFuture<ResponseEntity<?>> addAllActivityDataInSingleCall (ShipmentDetails shipmentDetails, ShipmentDetailsResponse shipmentDetailsResponse, Map<String, Object> masterDataResponse) {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        Set<String> activityTypes = new HashSet<>();
        if (!Objects.isNull(shipmentDetailsResponse.getAdditionalDetails()))
            activityTypes.addAll(masterDataUtils.createInBulkActivityTypeRequest(shipmentDetailsResponse.getAdditionalDetails(), AdditionalDetails.class, fieldNameKeyMap, AdditionalDetails.class.getSimpleName()) );

        Map v1Data = masterDataUtils.fetchInActivityMasterList(activityTypes.stream().toList());
        masterDataUtils.pushToCache(v1Data, CacheConstants.ACTIVITY_TYPE);

        if(masterDataResponse == null) {
            if (!Objects.isNull(shipmentDetailsResponse.getAdditionalDetails()))
                shipmentDetailsResponse.getAdditionalDetails().addTextData(masterDataUtils.setMasterData(fieldNameKeyMap.get(AdditionalDetails.class.getSimpleName()), CacheConstants.ACTIVITY_TYPE));
        }
        else {
            masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.ACTIVITY_TYPE, masterDataResponse);
        }

        return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(v1Data));
    }

    private CompletableFuture<ResponseEntity<?>> addAllSalesAgentInSingleCall (ShipmentDetails shipmentDetails, ShipmentDetailsResponse shipmentDetailsResponse, Map<String, Object> masterDataResponse) {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        Set<String> salesAgents = new HashSet<>();
        if (!Objects.isNull(shipmentDetailsResponse))
            salesAgents.addAll(masterDataUtils.createInBulkSalesAgentRequest(shipmentDetailsResponse, ShipmentDetails.class, fieldNameKeyMap, ShipmentDetails.class.getSimpleName()) );

        Map v1Data = masterDataUtils.fetchInSalesAgentList(salesAgents.stream().toList());
        masterDataUtils.pushToCache(v1Data, CacheConstants.SALES_AGENT);

        if(masterDataResponse == null) {
            if (!Objects.isNull(shipmentDetailsResponse))
                shipmentDetailsResponse.addTextData(masterDataUtils.setMasterData(fieldNameKeyMap.get(ShipmentDetails.class.getSimpleName()), CacheConstants.SALES_AGENT));
        }
        else {
            masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.SALES_AGENT, masterDataResponse);
        }

        return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(v1Data));
    }

    private CompletableFuture<ResponseEntity<?>> addAllContainerTypesInSingleCall(ShipmentDetails shipmentDetails, ShipmentDetailsResponse shipmentDetailsResponse, Map<String, Object> masterDataResponse) {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        List<String> containerTypes = new ArrayList<>();
        if (!Objects.isNull(shipmentDetailsResponse.getContainersList()))
            shipmentDetailsResponse.getContainersList().forEach(r -> containerTypes.addAll(masterDataUtils.createInBulkContainerTypeRequest(r, Containers.class, fieldNameKeyMap, Containers.class.getSimpleName() + r.getId() )));

        Map<String, EntityTransferContainerType> v1Data = masterDataUtils.fetchInBulkContainerTypes(containerTypes);
        masterDataUtils.pushToCache(v1Data, CacheConstants.CONTAINER_TYPE);

        if(masterDataResponse == null) {
            if (!Objects.isNull(shipmentDetailsResponse.getContainersList()))
                shipmentDetailsResponse.getContainersList().forEach(r -> r.setContainerCodeData(masterDataUtils.setMasterData(fieldNameKeyMap.get(Containers.class.getSimpleName() + r.getId()), CacheConstants.CONTAINER_TYPE)));
        }
        else {
            masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.CONTAINER_TYPE, masterDataResponse);
        }

        return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(v1Data));
    }

    private CompletableFuture<ResponseEntity<?>> addAllVesselDataInSingleCall(ShipmentDetails shipmentDetails, ShipmentDetailsResponse shipmentDetailsResponse, Map<String, Object> masterDataResponse) {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        List<String> vesselList = new ArrayList<>();
        if (!Objects.isNull(shipmentDetailsResponse.getBookingCarriagesList()))
            shipmentDetailsResponse.getBookingCarriagesList().forEach(r -> vesselList.addAll(masterDataUtils.createInBulkVesselsRequest(r, BookingCarriage.class, fieldNameKeyMap, BookingCarriage.class.getSimpleName() + r.getId() )));
        if (!Objects.isNull(shipmentDetailsResponse.getCarrierDetails()))
            vesselList.addAll((masterDataUtils.createInBulkVesselsRequest(shipmentDetailsResponse.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName() )));

        Map v1Data = masterDataUtils.fetchInBulkVessels(vesselList);
        masterDataUtils.pushToCache(v1Data, CacheConstants.VESSELS);

        if(masterDataResponse == null) {
            shipmentDetailsResponse.getCarrierDetails().setVesselsMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName()), CacheConstants.VESSELS));
        }
        else {
            masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.VESSELS, masterDataResponse);
        }

        return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(Arrays.asList()));
    }

    private CompletableFuture<ResponseEntity<?>> addAllDGSubstanceDataInSingleCall (ShipmentDetails shipmentDetails, ShipmentDetailsResponse shipmentDetailsResponse, Map<String, Object> masterDataResponse) {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        Set<String> dgSubstanceIdList = new HashSet<>();
        if (!Objects.isNull(shipmentDetailsResponse.getPackingList()))
            shipmentDetailsResponse.getPackingList().forEach(r -> dgSubstanceIdList.addAll(masterDataUtils.createInBulkDGSubstanceRequest(r, Packing.class, fieldNameKeyMap, Packing.class.getSimpleName() + r.getId() )));

        Map v1Data = masterDataUtils.fetchInDGSubstanceList(dgSubstanceIdList.stream().toList());
        masterDataUtils.pushToCache(v1Data, CacheConstants.DG_SUBSTANCES);

        if(masterDataResponse == null) { }
        else {
            masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.DG_SUBSTANCES, masterDataResponse);
        }

        return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(v1Data));
    }

    private void setContainersPacksAutoUpdateData (ShipmentDetailsResponse shipmentDetailsResponse) {
        List<PackingResponse> packings = shipmentDetailsResponse.getPackingList();
        List<ContainerResponse> containers = shipmentDetailsResponse.getContainersList();
        Map<Long, String> map = new HashMap<>();
        if(containers != null)
            map = containers.stream().collect(Collectors.toMap(cont -> cont.getId(), cont -> cont.getContainerNumber()));
        Map<Long, Map<String, String>> contMap = new HashMap<>();
        ShipmentSettingsDetails shipmentSettingsDetails = shipmentSettingsDao.getSettingsByTenantIds(List.of(TenantContext.getCurrentTenant())).get(0);
        boolean flag = shipmentDetailsResponse.getContainerAutoWeightVolumeUpdate() != null && shipmentDetailsResponse.getContainerAutoWeightVolumeUpdate().booleanValue()
                && shipmentSettingsDetails.getMultipleShipmentEnabled() != null && shipmentSettingsDetails.getMultipleShipmentEnabled();
        if(packings != null && packings.size() > 0) {
            for (PackingResponse pack : packings) {
                if(pack.getContainerId() != null) {
                    if(map.containsKey(pack.getContainerId()))
                        pack.setContainerNumber(map.get(pack.getContainerId()));
                    if(flag) {
                        if(!contMap.containsKey(pack.getContainerId())) {
                            Map<String, String> tempMap = new HashMap<>();
                            tempMap.put("handlingInfo", "");
                            tempMap.put("descriptionOfGoods", "");
                            contMap.put(pack.getContainerId(), tempMap);
                        }
                        String handlingInfo = contMap.get(pack.getContainerId()).get("handlingInfo");
                        String descriptionOfGoods = contMap.get(pack.getContainerId()).get("descriptionOfGoods");
                        if(!IsStringNullOrEmpty(pack.getHandlingInfo())) {
                            if (handlingInfo.length() == 0)
                                handlingInfo = pack.getHandlingInfo();
                            else
                                handlingInfo = handlingInfo + ", " + pack.getHandlingInfo();
                        }
                        if(!IsStringNullOrEmpty(pack.getGoodsDescription())) {
                            if(descriptionOfGoods.length() == 0)
                                descriptionOfGoods = pack.getGoodsDescription();
                            else
                                descriptionOfGoods = descriptionOfGoods + ", " + pack.getGoodsDescription();
                        }
                        contMap.get(pack.getContainerId()).put("handlingInfo", handlingInfo);
                        contMap.get(pack.getContainerId()).put("descriptionOfGoods", descriptionOfGoods);
                    }
                }
            }
        }
        if(containers != null && containers.size() > 0) {
            for(ContainerResponse container : containers) {
                if(flag) {
                    if(contMap.containsKey(container.getId())) {
                        container.setTextFieldData(contMap.get(container.getId()));
                    }
                    else {
                        Map<String, String> tempMap = new HashMap<>();
                        tempMap.put("handlingInfo", "");
                        tempMap.put("descriptionOfGoods", "");
                        container.setTextFieldData(tempMap);
                    }
                }
                else {
                    Map<String, String> tempMap = new HashMap<>();
                    tempMap.put("handlingInfo", container.getHandlingInfo());
                    tempMap.put("descriptionOfGoods", container.getDescriptionOfGoods());
                    container.setTextFieldData(tempMap);
                }
            }
        }
    }

    public ResponseEntity<?> cloneShipment(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Shipment retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.error("Request Id is null for Shipment retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(id);
            if (!shipmentDetails.isPresent()) {
                log.debug("Shipment Details is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            ShipmentRequest cloneShipmentDetails = jsonHelper.convertValue(shipmentDetails.get(), ShipmentRequest.class);
            cloneShipmentDetails.setHouseBill(null);
            cloneShipmentDetails.setBookingReference(null);
            cloneShipmentDetails.setContainersList(null);
            cloneShipmentDetails.setRoutingsList(null);
            cloneShipmentDetails.setShipmentId(null);
            cloneShipmentDetails.setMasterBill(null);
            cloneShipmentDetails.setConsolidationList(null);

            cloneShipmentDetails.setShipmentCreatedOn(LocalDateTime.now());
            
            if(Constants.TRANSPORT_MODE_SEA.equals(cloneShipmentDetails.getTransportMode()) && Constants.DIRECTION_EXP.equals(cloneShipmentDetails.getDirection()))
                cloneShipmentDetails.setHouseBill(generateCustomHouseBL(null));

            CommonRequestModel requestModel = CommonRequestModel.buildRequest(cloneShipmentDetails);
            log.info("Shipment details cloning started for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            ShipmentDetailsResponse response = jsonHelper.convertValue(cloneShipmentDetails, ShipmentDetailsResponse.class);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<?> transportInstructionList(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            TIListRequest tiListRequest = (TIListRequest) commonRequestModel.getData();
            if(tiListRequest == null) {
                log.error("Request is empty for TI List with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            if(tiListRequest.getShipmentGuid() == null) {
                log.error("Shipment Guid is null for TI List with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            V1DataResponse v1DataResponse = v1Service.fetchTransportInstructionList(tiListRequest);
            List<TIResponse> tiResponseList = jsonHelper.convertValueToList(v1DataResponse.entities, TIResponse.class);
            return ResponseHelper.buildSuccessResponse(tiResponseList);
        }
        catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<?> containerListForTI(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            TIContainerListRequest tiContainerListRequest = (TIContainerListRequest) commonRequestModel.getData();
            if(tiContainerListRequest == null) {
                log.error("Request is empty for container TI List with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            if(tiContainerListRequest.getShipmentGuid() == null) {
                log.error("Shipment Guid is null for conatiner TI List with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            V1DataResponse v1DataResponse = v1Service.fetchContainersListForTI(tiContainerListRequest);
            List<TIContainerResponse> containerResponseList = jsonHelper.convertValueToList(v1DataResponse.entities, TIContainerResponse.class);
            return ResponseHelper.buildSuccessResponse(containerResponseList);
        }
        catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<?> retrieveByOrderId(String orderId) {
        try {
            ShipmentDetailsResponse response = jsonHelper.convertValue(orderManagementAdapter.getOrder(orderId), ShipmentDetailsResponse.class);
            this.addAllMasterDataInSingleCall(null, response, null);
            this.addAllUnlocationDataInSingleCall(null, response, null);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e){
            throw new RunnerException(e.getMessage());
        }
    }

    @Override
    public ResponseEntity<?> generateCustomHouseBLNumber() {
        try {
            return ResponseHelper.buildSuccessResponse(GenerateCustomHblResponse.builder().hblNumber(generateCustomHouseBL(null)).build());
        } catch (Exception e) {
            throw new RunnerException(e.getMessage());
        }
    }

    @Override
    public ResponseEntity<?> getShipmentFromConsol(Long consolidationId) {
        List<ShipmentSettingsDetails> shipmentSettingsDetails = shipmentSettingsDao.getSettingsByTenantIds(List.of(TenantContext.getCurrentTenant()));
        if(shipmentSettingsDetails == null || shipmentSettingsDetails.size() == 0)
            throw new RunnerException("Shipment settings empty for current tenant");
        var tenantSettings = shipmentSettingsDetails.get(0);
        // Populate shipment details on basis of tenant settings

        ShipmentDetailsResponse shipment;
        var consolidationResponse = consolidationDetailsDao.findById(consolidationId);

        if (consolidationResponse.isEmpty())
            throw new DataRetrievalFailureException("Failed to fetch the consolidation with id " + consolidationId);

        var consolidation = modelMapper.map(consolidationResponse.get(), ConsolidationDetailsResponse.class);

        var origin = consolidation.getCarrierDetails() != null ? consolidation.getCarrierDetails().getOrigin() : null;
        var destination = consolidation.getCarrierDetails() != null ? consolidation.getCarrierDetails().getDestination() : null;
        var originPort = consolidation.getCarrierDetails() != null ? consolidation.getCarrierDetails().getOriginPort() : null;
        var destinationPort = consolidation.getCarrierDetails() != null ? consolidation.getCarrierDetails().getDestinationPort() : null;
        var voyage = consolidation.getCarrierDetails() != null ? consolidation.getCarrierDetails().getVoyage() : null;
        var vessel = consolidation.getCarrierDetails() != null ? consolidation.getCarrierDetails().getVessel() : null;
        var aircrafType = consolidation.getCarrierDetails() != null ? consolidation.getCarrierDetails().getAircraftType() : null;
        var eta = consolidation.getCarrierDetails() != null ? consolidation.getCarrierDetails().getEta() : null;
        var etd = consolidation.getCarrierDetails() != null ? consolidation.getCarrierDetails().getEtd() : null;
        var ata = consolidation.getCarrierDetails() != null ? consolidation.getCarrierDetails().getAta() : null;
        var atd = consolidation.getCarrierDetails() != null ? consolidation.getCarrierDetails().getAtd() : null;
        var consolAllocation = consolidation.getAllocations();
        var consolCarrier = consolidation.getCarrierDetails();
        shipment = ShipmentDetailsResponse.builder()
                .transportMode(consolidation.getTransportMode() == null ? tenantSettings.getDefaultTransportMode() : consolidation.getTransportMode())
                .bookingNumber(consolidation.getBookingNumber())
                .consolidationList(List.of(modelMapper.map(consolidation, ConsolidationListResponse.class)))
                .direction(consolidation.getShipmentType() == null ? tenantSettings.getDefaultShipmentType() : consolidation.getShipmentType())
                .jobType(Constants.SHIPMENT_TYPE_STD)
                .shipmentType(consolidation.getContainerCategory() == null ? tenantSettings.getDefaultContainerType() : consolidation.getContainerCategory())
                .additionalDetails(AdditionalDetailResponse.builder()
                        .SMTPIGMDate(consolidation.getSmtpigmDate())
                        .SMTPIGMNumber(consolidation.getSmtpigmNumber())
                        .inwardDateAndTime(consolidation.getInwardDateAndTime())
                        .releaseType(consolidation.getReleaseType())
                        .warehouseId(consolidation.getWarehouseId())
                        .isInland(consolidation.getIsInland())
                        .original(consolidation.getOriginal())
                        .IGMFileNo(consolidation.getIgmFileNo())
                        .IGMFileDate(consolidation.getIgmFileDate())
                        .IGMInwardDate(consolidation.getIgmInwardDate())
                        .copy(consolidation.getCopy())
                        .customDeclType(consolidation.getDeclarationType())
                        .build())
                .carrierDetails(CarrierDetailResponse.builder()
                        .ata(ata)
                        .eta(eta)
                        .atd(atd)
                        .etd(etd)
                        .origin(origin)
                        .vessel(vessel)
                        .originPort(originPort)
                        .destinationPort(destinationPort)
                        .shippingLine(consolCarrier != null ? consolCarrier.getShippingLine() : null)
                        .voyage(voyage)
                        .aircraftType(aircrafType)
                        .destination(destination)
                        .flightNumber(consolCarrier != null ? consolCarrier.getFlightNumber() : null)
                        .build())
                .weight(consolAllocation != null ? consolAllocation.getWeight() : null)
                .weightUnit(consolAllocation != null ? consolAllocation.getWeightUnit() : tenantSettings.getWeightChargeableUnit())
                .volume(consolAllocation != null ? consolAllocation.getVolume() : null)
                .volumeUnit(consolAllocation != null ? consolAllocation.getVolumeUnit() : tenantSettings.getVolumeChargeableUnit())
                .chargable(consolAllocation != null ? consolAllocation.getChargable() : null)
                .chargeableUnit(consolAllocation != null ? consolAllocation.getChargeableUnit() : null)
                .paymentTerms(consolidation.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) && consolidation.getShipmentType().equals("EXP")
                        ? consolidation.getPayment() : null)
                .masterBill(consolidation.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) ? consolidation.getMawb() : consolidation.getBol())
                .status(0)
                .source(Constants.SYSTEM)
                .createdBy(UserContext.getUser().getUsername())
                .customerCategory(CustomerCategoryRates.CATEGORY_5)
                .shipmentCreatedOn(LocalDateTime.now())
                .consolRef(consolidation.getConsolidationNumber())
                .build();

        if (consolidation.getConsolidationAddresses() != null) {
            consolidation.getConsolidationAddresses().stream().forEach(party -> {
                if (party.getType().equals("NP1")) {
                    shipment.getAdditionalDetails().setNotifyParty(
                            PartiesResponse.builder()
                                    .orgCode(party.getOrgCode())
                                    .addressCode(party.getAddressCode())
                                    .build());
                }
            });
        }

        if(!IsStringNullOrEmpty(shipment.getCarrierDetails().getOrigin())) {
            if(IsStringNullOrEmpty(shipment.getAdditionalDetails().getPaidPlace()))
                shipment.getAdditionalDetails().setPaidPlace(shipment.getCarrierDetails().getOrigin());
            if(IsStringNullOrEmpty(shipment.getAdditionalDetails().getPlaceOfIssue()))
                shipment.getAdditionalDetails().setPlaceOfIssue(shipment.getCarrierDetails().getOrigin());
            if(IsStringNullOrEmpty(shipment.getAdditionalDetails().getPlaceOfSupply()))
                shipment.getAdditionalDetails().setPlaceOfSupply(shipment.getCarrierDetails().getOrigin());
        }
        if(shipment.getCarrierDetails().getEta() != null) {
            if(shipment.getAdditionalDetails().getDateOfIssue() == null)
                shipment.getAdditionalDetails().setDateOfIssue(shipment.getCarrierDetails().getEta());
            if(shipment.getAdditionalDetails().getDateOfReceipt() == null)
                shipment.getAdditionalDetails().setDateOfReceipt(shipment.getCarrierDetails().getEta());
        }

        //Generate HBL
        if(Constants.TRANSPORT_MODE_SEA.equals(shipment.getTransportMode()) && Constants.DIRECTION_EXP.equals(shipment.getDirection()))
            shipment.setHouseBill(generateCustomHouseBL(null));

        try {
            log.info("Fetching Tenant Model");
            TenantModel tenantModel = modelMapper.map(v1Service.retrieveTenant().getEntity(), TenantModel.class);
            String currencyCode = tenantModel.currencyCode;
            shipment.setFreightLocalCurrency(currencyCode);
        } catch (Exception e){
            log.error("Failed in fetching tenant data from V1 with error : {}", e);
        }

        createShipmentPayload(modelMapper.map(shipment, ShipmentDetails.class), shipment);

        return ResponseHelper.buildSuccessResponse(shipment);
    }

    public String generateCustomHouseBL(ShipmentDetails shipmentDetails) {
        String res = null;
        if(shipmentDetails != null) {
            res = shipmentDetails.getHouseBill();
        }
        List<ShipmentSettingsDetails> shipmentSettingsDetailsList = shipmentSettingsDao.getSettingsByTenantIds(List.of(TenantContext.getCurrentTenant()));
        ShipmentSettingsDetails tenantSetting = null;
        if (shipmentSettingsDetailsList.get(0) != null)
            tenantSetting = shipmentSettingsDetailsList.get(0);
        if(shipmentDetails == null && tenantSetting != null && tenantSetting.getRestrictHblGen()) {
            return null;
        }

        if (shipmentDetails != null && tenantSetting.getRestrictHblGen() && tenantSetting.getCustomisedSequence()) {

            try {
                res = productEngine.getCustomizedBLNumber(shipmentDetails, tenantSetting);
                // generate via Product Identifier Utility
                // res = someMethod();
            } catch (Exception e) {
                log.error(e.getMessage());
            }
        }

        if(res == null || res.isEmpty()) {
            res = tenantSetting.getHousebillPrefix() ==  null ? "" : tenantSetting.getHousebillPrefix();
            String numberGeneration = tenantSetting.getHousebillNumberGeneration() ==  null ? "" : tenantSetting.getHousebillNumberGeneration();
            switch(numberGeneration) {
                case "Random" :
                    res += StringUtility.getRandomString(10);
                    break;
                case "Serial" :
                    String serialNumber = v1Service.getMaxShipmentId();
//                    Long serialNumber = shipmentDao.findMaxId() + 1;
                    res += serialNumber;
                    break;
                default : res = "";
                    break;
            }
        }

        return res;
    }

    @Override
    public ResponseEntity<?> getDefaultShipment() {
        String responseMsg;
        try {
            List<ShipmentSettingsDetails> shipmentSettingsDetails = shipmentSettingsDao.getSettingsByTenantIds(List.of(TenantContext.getCurrentTenant()));
            if(shipmentSettingsDetails == null || shipmentSettingsDetails.size() == 0)
                throw new RunnerException("Shipment settings empty for current tenant");
            var tenantSettings = shipmentSettingsDetails.get(0);
            // Populate shipment details on basis of tenant settings
            ShipmentDetailsResponse response = new ShipmentDetailsResponse();
            response.setAdditionalDetails(new AdditionalDetailResponse());
            response.setCarrierDetails(new CarrierDetailResponse());
            response.setTransportMode(tenantSettings.getDefaultTransportMode());
            response.setDirection(tenantSettings.getDefaultShipmentType());
            response.setShipmentType(tenantSettings.getDefaultContainerType());

            response.setVolumeUnit(tenantSettings.getVolumeChargeableUnit());
            response.setWeightUnit(tenantSettings.getWeightChargeableUnit());
            response.setStatus(0);
            response.setSource(Constants.SYSTEM);
            response.setCreatedBy(UserContext.getUser().getUsername());
            response.setCustomerCategory(CustomerCategoryRates.CATEGORY_5);
            response.setShipmentCreatedOn(LocalDateTime.now());
            //Generate HBL
            if(Constants.TRANSPORT_MODE_SEA.equals(response.getTransportMode()) && Constants.DIRECTION_EXP.equals(response.getDirection()))
                response.setHouseBill(generateCustomHouseBL(null));

            try {
                log.info("Fetching Tenant Model");
                TenantModel tenantModel = modelMapper.map(v1Service.retrieveTenant().getEntity(), TenantModel.class);
                String currencyCode = tenantModel.currencyCode;
                response.setFreightLocalCurrency(currencyCode);
            } catch (Exception e){
                log.error("Failed in fetching tenant data from V1 with error : {}", e);
            }

            if(Constants.TRANSPORT_MODE_SEA.equals(response.getTransportMode()) && Constants.DIRECTION_EXP.equals(response.getDirection()))
                response.setHouseBill(generateCustomHouseBL(null));

            this.addAllMasterDataInSingleCall(null, response, null);

            return ResponseHelper.buildSuccessResponse(response);
        } catch(Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<?> getMasterDataMappings() {
        String responseMsg;
        try {
            List<MasterDataDescriptionResponse> response = new ArrayList<>();

            //Get current Tenant's setting
            Optional<ShipmentSettingsDetails> optional = shipmentSettingsDao.findByTenantId(TenantContext.getCurrentTenant());
            var tenantSetting = optional.get();
            // get all the master data based on field names of
            response = masterDataUtils.getMasterDataDescription(tenantSetting);

            return  ResponseHelper.buildSuccessResponse(response);
        } catch(Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<?> attachListShipment(CommonRequestModel commonRequestModel){
        AttachListShipmentRequest request = (AttachListShipmentRequest) commonRequestModel.getData();

        Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findById(request.getConsolidationId());
        if (!consolidationDetails.isPresent()) {
            log.debug("Consolidation Details is null for Id {} with Request Id {}", request.getConsolidationId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        request.setIncludeTbls(Arrays.asList("additionalDetails", "client", "consigner", "consignee", "carrierDetails", "pickupDetails", "deliveryDetails"));
        ListCommonRequest listRequest = setCrieteriaForAttachShipment(request, consolidationDetails.get());
        Pair<Specification<ShipmentDetails>, Pageable> tuple = fetchData(listRequest, ShipmentDetails.class, tableNames);

        Page<ShipmentDetails> shipmentDetailsPage = shipmentDao.findAll(tuple.getLeft().and(notInMappingTable()), tuple.getRight());
        return ResponseHelper.buildListSuccessResponse(
                convertEntityListToDtoList(shipmentDetailsPage.getContent()),
                shipmentDetailsPage.getTotalPages(),
                shipmentDetailsPage.getTotalElements());
    }
    public static Specification<ShipmentDetails> notInMappingTable() {
        return (root, query, criteriaBuilder) -> {
            return criteriaBuilder.isEmpty(root.get("consolidationList"));
        };
    }

    private ListCommonRequest setCrieteriaForAttachShipment(AttachListShipmentRequest request, ConsolidationDetails consolidationDetails) {
        if(request.getFilterCriteria() != null && request.getFilterCriteria().isEmpty()){
            request.setFilterCriteria(Arrays.asList(FilterCriteria.builder().innerFilter(new ArrayList<>()).build()));
        }
        ListCommonRequest defaultRequest;
        defaultRequest = CommonUtils.andCriteria("transportMode", consolidationDetails.getTransportMode(), "=", request);
        if(!Objects.isNull(consolidationDetails.getCarrierDetails().getOriginPort()))
            CommonUtils.andCriteria("originPort", consolidationDetails.getCarrierDetails().getOriginPort(), "=", defaultRequest);
        else
            CommonUtils.andCriteria("originPort", "", "ISNULL", defaultRequest);
        if(!Objects.isNull(consolidationDetails.getCarrierDetails().getDestinationPort()))
            CommonUtils.andCriteria("destinationPort", consolidationDetails.getCarrierDetails().getDestinationPort(), "=", defaultRequest);
        else
            CommonUtils.andCriteria("destinationPort", "", "ISNULL", defaultRequest);
        if(!Objects.isNull(consolidationDetails.getShipmentType()))
            CommonUtils.andCriteria("direction", consolidationDetails.getShipmentType(), "=", defaultRequest);
        else
            CommonUtils.andCriteria("direction", "", "ISNULL", defaultRequest);
        CommonUtils.andCriteria("status", 2, "!=", defaultRequest);
        CommonUtils.andCriteria("status", 3, "!=", defaultRequest);
        List<FilterCriteria> criterias = defaultRequest.getFilterCriteria();
        List<FilterCriteria> innerFilters = criterias.get(0).getInnerFilter();
        Criteria criteria = Criteria.builder().fieldName("transportMode").operator("!=").value(Constants.TRANSPORT_MODE_AIR).build();
        FilterCriteria filterCriteria = FilterCriteria.builder().criteria(criteria).build();
        List<FilterCriteria> innerFilers1 = new ArrayList<>();
        innerFilers1.add(filterCriteria);
        criteria = Criteria.builder().fieldName("jobType").operator("!=").value(Constants.SHIPMENT_TYPE_DRT).build();
        filterCriteria = FilterCriteria.builder().criteria(criteria).logicOperator("or").build();
        innerFilers1.add(filterCriteria);
        criteria = Criteria.builder().fieldName("jobType").operator("ISNULL").build();
        filterCriteria = FilterCriteria.builder().criteria(criteria).logicOperator("or").build();
        innerFilers1.add(filterCriteria);
        filterCriteria = FilterCriteria.builder().logicOperator("and").innerFilter(innerFilers1).build();
        innerFilters.add(filterCriteria);

        if(request.getEtaMatch()){
            innerFilers1 = new ArrayList<>();
            if(!Objects.isNull(consolidationDetails.getCarrierDetails().getEta()))
                criteria = Criteria.builder().fieldName("eta").operator("=").value(consolidationDetails.getCarrierDetails().getEta()).build();
            else
                criteria = Criteria.builder().fieldName("eta").operator("ISNULL").build();
            filterCriteria = FilterCriteria.builder().criteria(criteria).build();
            innerFilers1.add(filterCriteria);
            criteria = Criteria.builder().fieldName("eta").operator("ISNULL").build();
            filterCriteria = FilterCriteria.builder().criteria(criteria).logicOperator("or").build();
            innerFilers1.add(filterCriteria);
            filterCriteria = FilterCriteria.builder().logicOperator("and").innerFilter(innerFilers1).build();
            innerFilters.add(filterCriteria);
        }
        if(request.getEtdMatch()){
            innerFilers1 = new ArrayList<>();
            if(!Objects.isNull(consolidationDetails.getCarrierDetails().getEtd()))
                criteria = Criteria.builder().fieldName("etd").operator("=").value(consolidationDetails.getCarrierDetails().getEtd()).build();
            else
                criteria = Criteria.builder().fieldName("etd").operator("ISNULL").build();
            filterCriteria = FilterCriteria.builder().criteria(criteria).build();
            innerFilers1.add(filterCriteria);
            criteria = Criteria.builder().fieldName("etd").operator("ISNULL").build();
            filterCriteria = FilterCriteria.builder().criteria(criteria).logicOperator("or").build();
            innerFilers1.add(filterCriteria);
            filterCriteria = FilterCriteria.builder().logicOperator("and").innerFilter(innerFilers1).build();
            innerFilters.add(filterCriteria);
        }
        if(request.getScheduleMatch()){
            if(consolidationDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR)){
                innerFilers1 = new ArrayList<>();
                if(!Objects.isNull(consolidationDetails.getCarrierDetails().getFlightNumber()))
                    criteria = Criteria.builder().fieldName("flightNumber").operator("=").value(consolidationDetails.getCarrierDetails().getFlightNumber()).build();
                else
                    criteria = Criteria.builder().fieldName("flightNumber").operator("ISNULL").build();
                filterCriteria = FilterCriteria.builder().criteria(criteria).build();
                innerFilers1.add(filterCriteria);
                criteria = Criteria.builder().fieldName("flightNumber").operator("ISNULL").build();
                filterCriteria = FilterCriteria.builder().criteria(criteria).logicOperator("or").build();
                innerFilers1.add(filterCriteria);
                filterCriteria = FilterCriteria.builder().logicOperator("and").innerFilter(innerFilers1).build();
                innerFilters.add(filterCriteria);

                innerFilers1 = new ArrayList<>();
                if(!Objects.isNull(consolidationDetails.getCarrierDetails().getShippingLine()))
                    criteria = Criteria.builder().fieldName("shippingLine").operator("=").value(consolidationDetails.getCarrierDetails().getShippingLine()).build();
                else
                    criteria = Criteria.builder().fieldName("shippingLine").operator("ISNULL").build();
                filterCriteria = FilterCriteria.builder().criteria(criteria).build();
                innerFilers1.add(filterCriteria);
                criteria = Criteria.builder().fieldName("shippingLine").operator("ISNULL").build();
                filterCriteria = FilterCriteria.builder().criteria(criteria).logicOperator("or").build();
                innerFilers1.add(filterCriteria);
                filterCriteria = FilterCriteria.builder().logicOperator("and").innerFilter(innerFilers1).build();
                innerFilters.add(filterCriteria);
            }
            else if(consolidationDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA)){
                innerFilers1 = new ArrayList<>();
                if(!Objects.isNull(consolidationDetails.getCarrierDetails().getVessel()))
                    criteria = Criteria.builder().fieldName("vessel").operator("=").value(consolidationDetails.getCarrierDetails().getVessel()).build();
                else
                    criteria = Criteria.builder().fieldName("vessel").operator("ISNULL").build();
                filterCriteria = FilterCriteria.builder().criteria(criteria).build();
                innerFilers1.add(filterCriteria);
                criteria = Criteria.builder().fieldName("vessel").operator("ISNULL").build();
                filterCriteria = FilterCriteria.builder().criteria(criteria).logicOperator("or").build();
                innerFilers1.add(filterCriteria);
                filterCriteria = FilterCriteria.builder().logicOperator("and").innerFilter(innerFilers1).build();
                innerFilters.add(filterCriteria);

                innerFilers1 = new ArrayList<>();
                if(!Objects.isNull(consolidationDetails.getCarrierDetails().getVoyage()))
                    criteria = Criteria.builder().fieldName("voyage").operator("=").value(consolidationDetails.getCarrierDetails().getVoyage()).build();
                else
                    criteria = Criteria.builder().fieldName("voyage").operator("ISNULL").build();
                filterCriteria = FilterCriteria.builder().criteria(criteria).build();
                innerFilers1.add(filterCriteria);
                criteria = Criteria.builder().fieldName("voyage").operator("ISNULL").build();
                filterCriteria = FilterCriteria.builder().criteria(criteria).logicOperator("or").build();
                innerFilers1.add(filterCriteria);
                filterCriteria = FilterCriteria.builder().logicOperator("and").innerFilter(innerFilers1).build();
                innerFilters.add(filterCriteria);
            }
        }
        return defaultRequest;
    }

    /**
     * MBL update, propagate the new value to the attached console and its linked shipments
     * @param shipment
     * @param oldMasterBill
     */
    private void updateMasterBill(ShipmentDetails shipment, String oldMasterBill) {
        var masterBill = shipment.getMasterBill();
        if(masterBill != null && (oldMasterBill == null || !masterBill.equals(oldMasterBill))) {
            List<ConsolidationDetails> consolidationList = shipment.getConsolidationList();
            var linkedConsol = (consolidationList != null && consolidationList.size() > 0) ? consolidationList.get(0) : null;
            if(linkedConsol != null) {
                linkedConsol.setBol(masterBill);
                List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByConsolidationId(linkedConsol.getId());
                List<Long> shipmentIdList = consoleShipmentMappings.stream().map(i -> i.getShipmentId()).collect(Collectors.toList());
                ListCommonRequest listReq = constructListCommonRequest("id", shipmentIdList, "IN");
                Pair<Specification<ShipmentDetails>, Pageable> pair = fetchData(listReq, ShipmentDetails.class, tableNames);
                Page<ShipmentDetails> page = shipmentDao.findAll(pair.getLeft(), pair.getRight());

                List<ShipmentDetails> shipments = page.getContent();
                shipments.stream()
                        .map(i -> i.setMasterBill(masterBill)).toList();
                consolidationDetailsDao.save(linkedConsol, false);
            }
        }
    }

    /**
     * back flows data of the current updated shipment to all its sibling shipments attached to the common console
     * @param shipment
     * @param oldMasterBill
     */
    private void updateLinkedShipmentData(ShipmentDetails shipment, ShipmentDetails oldEntity) {
        List<ConsolidationDetails> consolidationList = shipment.getConsolidationList();
        var linkedConsol = (consolidationList != null && consolidationList.size() > 0) ? consolidationList.get(0) : null;
        if(linkedConsol != null && (oldEntity == null || (shipment.getCarrierDetails() != null && oldEntity.getCarrierDetails() != null &&
                (!Objects.equals(shipment.getDirection(),oldEntity.getDirection()) ||
                        !Objects.equals(shipment.getCarrierDetails().getVoyage(),oldEntity.getCarrierDetails().getVoyage()) ||
                        !Objects.equals(shipment.getCarrierDetails().getVessel(),oldEntity.getCarrierDetails().getVessel()) ||
                        !Objects.equals(shipment.getCarrierDetails().getShippingLine(),oldEntity.getCarrierDetails().getShippingLine()))))) {
            List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByConsolidationId(linkedConsol.getId());
            List<Long> shipmentIdList = consoleShipmentMappings.stream().map(i -> i.getShipmentId()).collect(Collectors.toList());
            ListCommonRequest listReq = constructListCommonRequest("id", shipmentIdList, "IN");
            Pair<Specification<ShipmentDetails>, Pageable> pair = fetchData(listReq, ShipmentDetails.class, tableNames);
            Page<ShipmentDetails> page = shipmentDao.findAll(pair.getLeft(), pair.getRight());

            List<ShipmentDetails> shipments = page.getContent();
            shipments.stream()
              .map(i -> {
                  i.setDirection(shipment.getDirection());
                  if (shipment.getCarrierDetails() != null) {
                      i.getCarrierDetails().setVoyage(shipment.getCarrierDetails().getVoyage());
                      i.getCarrierDetails().setVessel(shipment.getCarrierDetails().getVessel());
                      i.getCarrierDetails().setShippingLine(shipment.getCarrierDetails().getShippingLine());
                  }
                  return i;
              }).toList();

            shipmentDao.saveAll(shipments);
        }
    }

    @Override
    public ResponseEntity<?> getIdFromGuid(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Shipment retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getGuid() == null) {
                log.error("Request Guid Id is null for Shipment retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            Optional<ShipmentDetails> shipmentDetails = shipmentDao.findByGuid(UUID.fromString(request.getGuid()));
            if (!shipmentDetails.isPresent()) {
                log.debug("Shipment Details is null for Guid {} with Request Id {}", request.getGuid(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("Shipment details fetched successfully for Guid {} with Request Id {}", request.getGuid(), LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildSuccessResponse(ShipmentDetailsResponse.builder().id(shipmentDetails.get().getId()).build());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private void autoGenerateEvents(ShipmentDetails shipmentDetails, Integer previousStauts) {
        Events response = null;
        if(shipmentDetails.getStatus() != null) {
            if (previousStauts == null || !shipmentDetails.getStatus().equals(previousStauts)) {
                if (shipmentDetails.getStatus().equals(ShipmentStatus.Confirmed.getValue())) {
                    response = createAutomatedEvents(shipmentDetails, Constants.SHPCNFRM);
                }
                if (shipmentDetails.getStatus().equals(ShipmentStatus.Completed.getValue())) {
                    response = createAutomatedEvents(shipmentDetails, Constants.SHPCMPLT);
                }
            }
            if(response != null) {
                if (shipmentDetails.getEventsList() == null)
                    shipmentDetails.setEventsList(new ArrayList<>());
                shipmentDetails.getEventsList().add(response);
            }
        }
    }

    private Events createAutomatedEvents(ShipmentDetails shipmentDetails, String eventCode) {
        Events events = new Events();
        // Set event fields from shipment
        events.setActual(LocalDateTime.now());
        events.setEstimated(LocalDateTime.now());
        events.setSource(Constants.CARGO_RUNNER);
        events.setIsPublicTrackingEvent(true);
        events.setEntityType(Constants.SHIPMENT);
        events.setEntityId(shipmentDetails.getId());
        events.setTenantId(TenantContext.getCurrentTenant());
        events.setEventCode(eventCode);
        // Persist the event
        eventDao.save(events);
        return events;
    }

    public ResponseEntity<?> fetchShipmentsForConsoleId(CommonRequestModel commonRequestModel) {
        CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
        if(request.getId() == null) {
            log.error("Request Id is null for Consolidation retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new RunnerException("Id can't be null");
        }
        Long id = request.getId();
        List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByConsolidationId(id);
        List<Long> shipmentIdsList = new ArrayList<>();
        if(consoleShipmentMappings != null && consoleShipmentMappings.size() > 0) {
            shipmentIdsList = consoleShipmentMappings.stream().map(x -> x.getShipmentId()).collect(Collectors.toList());
        }
        ListCommonRequest listCommonRequest = CommonUtils.andCriteria("id", shipmentIdsList, "IN", null);
        return fetchShipments(CommonRequestModel.buildRequest(listCommonRequest));
    }

}
