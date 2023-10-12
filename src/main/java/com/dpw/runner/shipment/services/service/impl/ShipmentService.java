package com.dpw.runner.shipment.services.service.impl;


import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.*;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerPartialListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.CarrierListObject;
import com.dpw.runner.shipment.services.dto.patchRequest.ShipmentPatchRequest;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.dto.v1.response.V1ContainerTypeResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.GenerationType;
import com.dpw.runner.shipment.services.entitytransfer.dto.*;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.mapper.ShipmentDetailsMapper;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.masterdata.response.CarrierResponse;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service_bus.AzureServiceBusTopic;
import com.dpw.runner.shipment.services.service_bus.ISBProperties;
import com.dpw.runner.shipment.services.service_bus.SBUtilsImpl;
import com.dpw.runner.shipment.services.syncing.impl.ShipmentSync;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.utils.PartialFetchUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.interceptor.TransactionAspectSupport;
import org.springframework.transaction.support.TransactionTemplate;
import org.springframework.util.StringUtils;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.*;

@SuppressWarnings("ALL")
@Service
@Slf4j
public class ShipmentService implements IShipmentService {
    @Autowired
    private ModelMapper modelMapper;

    @Autowired
    private ObjectMapper objectMapper;
    @Autowired
    private ShipmentDetailsMapper shipmentDetailsMapper;

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
    private IMawbStocksLinkDao mawbStocksLinkDao;

    @Autowired
    private IMawbStocksDao mawbStocksDao;

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
    private SBUtilsImpl sbUtils;

    @Autowired
    private ISBProperties isbProperties;

    @Autowired
    private AzureServiceBusTopic azureServiceBusTopic;
    @Autowired
    private MasterDataUtils masterDataUtils;

    @Autowired
    private AuditLogService auditLogService;
    @Autowired
    ShipmentSync shipmentSync;

    @Autowired
    private CommonUtils commonUtils;

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
            Map.entry("clientOrgCode", RunnerEntityMapping.builder().tableName("client").dataType(String.class).fieldName("orgCode").build()),
            Map.entry("consignerOrgCode", RunnerEntityMapping.builder().tableName("consigner").dataType(String.class).fieldName("orgCode").build()),
            Map.entry("consigneeOrgCode", RunnerEntityMapping.builder().tableName("consignee").dataType(String.class).fieldName("orgCode").build()),
            Map.entry("clientAddressCode", RunnerEntityMapping.builder().tableName("client").dataType(Integer.class).fieldName("addressCode").build()),
            Map.entry("consignerAddressCode", RunnerEntityMapping.builder().tableName("consigner").dataType(String.class).fieldName("addressCode").build()),
            Map.entry("consigneeAddressCode", RunnerEntityMapping.builder().tableName("consignee").dataType(String.class).fieldName("addressCode").build()),
            Map.entry("houseBill", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("houseBill").build()),
            Map.entry("hblType", RunnerEntityMapping.builder().tableName("additionalDetails").dataType(String.class).fieldName("hblType").build()),
            Map.entry("transportMode", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("transportMode").build()),
            Map.entry("releaseType", RunnerEntityMapping.builder().tableName("additionalDetails").dataType(String.class).fieldName("releaseType").build()),
            Map.entry("deliveryMode", RunnerEntityMapping.builder().tableName("additionalDetails").dataType(String.class).fieldName("deliveryMode").build()),
            Map.entry("direction", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("direction").build()),
            Map.entry("shipmentType", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("shipmentType").build()),
            Map.entry("status", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(Integer.class).fieldName("status").build()),
            Map.entry("source", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("source").build()),
            Map.entry("jobType", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("jobType").build()),
            Map.entry("createdBy", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("createdBy").build()),
            Map.entry("serviceType", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("serviceType").build()),
            Map.entry("masterBill", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("masterBill").build()),
            Map.entry("bookingReference", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("bookingReference").build()),
            Map.entry("consolRef", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("consolRef").build()),
            Map.entry("salesAgent", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(Long.class).fieldName("salesAgent").build()),
            Map.entry("paymentTerms", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("paymentTerms").build()),
            Map.entry("incoterms", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("incoterms").build()),
            Map.entry("shipmentId", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("shipmentId").build()),
            Map.entry("isDomestic", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(Boolean.class).fieldName("isDomestic").build()),
            Map.entry("assignedTo", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(Integer.class).fieldName("assignedTo").build()),
            Map.entry("additionalTerms", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("additionalTerms").build()),
            Map.entry("goodsDescription", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("goodsDescription").build()),
            Map.entry("createdAt", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(LocalDateTime.class).fieldName("createdAt").build()),
            Map.entry("updatedAt", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(LocalDateTime.class).fieldName("updatedAt").build()),
            Map.entry("deliveryEstimated", RunnerEntityMapping.builder().tableName("deliveryDetails").dataType(LocalDateTime.class).fieldName("estimatedPickupOrDelivery").build()),
            Map.entry("deliveryActual", RunnerEntityMapping.builder().tableName("deliveryDetails").dataType(LocalDateTime.class).fieldName("actualPickupOrDelivery").build()),
            Map.entry("deliveryRequiredBy", RunnerEntityMapping.builder().tableName("deliveryDetails").dataType(LocalDateTime.class).fieldName("requiredBy").build()),
            Map.entry("pickupEstimated", RunnerEntityMapping.builder().tableName("pickupDetails").dataType(LocalDateTime.class).fieldName("estimatedPickupOrDelivery").build()),
            Map.entry("pickupActual", RunnerEntityMapping.builder().tableName("pickupDetails").dataType(LocalDateTime.class).fieldName("actualPickupOrDelivery").build()),
            Map.entry("pickupRequiredBy", RunnerEntityMapping.builder().tableName("pickupDetails").dataType(LocalDateTime.class).fieldName("requiredBy").build()),
            Map.entry("screeningStatus", RunnerEntityMapping.builder().tableName("additionalDetails").dataType(String.class).fieldName("screeningStatus").build()),
            Map.entry("paidPlace", RunnerEntityMapping.builder().tableName("additionalDetails").dataType(Long.class).fieldName("paidPlace").build()),
            Map.entry("placeOfIssue", RunnerEntityMapping.builder().tableName("additionalDetails").dataType(Long.class).fieldName("placeOfIssue").build()),
            Map.entry("dateOfIssue", RunnerEntityMapping.builder().tableName("additionalDetails").dataType(LocalDateTime.class).fieldName("dateOfIssue").build()),
            Map.entry("dateOfReceipt", RunnerEntityMapping.builder().tableName("additionalDetails").dataType(LocalDateTime.class).fieldName("dateOfReceipt").build()),
            Map.entry("goodsCo", RunnerEntityMapping.builder().tableName("additionalDetails").dataType(String.class).fieldName("goodsCo").build()),
            Map.entry("boeDate", RunnerEntityMapping.builder().tableName("additionalDetails").dataType(LocalDateTime.class).fieldName("boeDate").build()),
            Map.entry("boeNumber", RunnerEntityMapping.builder().tableName("additionalDetails").dataType(String.class).fieldName("boeNumber").build()),
            Map.entry("shippingLine", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(String.class).fieldName("shippingLine").build()),
            Map.entry("vessel", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(String.class).fieldName("vessel").build()),
            Map.entry("voyage", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(String.class).fieldName("voyage").build()),
            Map.entry("origin", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(String.class).fieldName("origin").build()),
            Map.entry("destination", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(String.class).fieldName("destination").build()),
            Map.entry("eta", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(LocalDateTime.class).fieldName("eta").build()),
            Map.entry("etd", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(LocalDateTime.class).fieldName("etd").build()),
            Map.entry("ata", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(LocalDateTime.class).fieldName("ata").build()),
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
            Map.entry("id", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(Long.class).fieldName("id").build())
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

            shipmentDetail = shipmentDao.save(shipmentDetail);
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
            responseList.add(response);
        });
        setLocationData(responseList);
        setContainerTeu(lst, responseList);
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

    private void setLocationData(List<IRunnerResponse> responseList) {
        Set<String> locCodes = new HashSet<>();
        for (IRunnerResponse response : responseList) {
            if (((ShipmentListResponse) response).getCarrierDetails() != null) {
                if (StringUtility.isNotEmpty(((ShipmentListResponse) response).getCarrierDetails().getOriginPort())) {
                    locCodes.add(((ShipmentListResponse) response).getCarrierDetails().getOriginPort());
                }
                if (StringUtility.isNotEmpty(((ShipmentListResponse) response).getCarrierDetails().getDestinationPort())) {
                    locCodes.add(((ShipmentListResponse) response).getCarrierDetails().getDestinationPort());
                }
            }
        }

        if (locCodes.size() > 0) {
            List<Object> criteria = Arrays.asList(
                    Arrays.asList("LocCode"),
                    "In",
                    Arrays.asList(locCodes)
            );
            CommonV1ListRequest commonV1ListRequest = CommonV1ListRequest.builder().skip(0).take(0).criteriaRequests(criteria).build();
            V1DataResponse v1DataResponse = v1Service.fetchUnlocation(commonV1ListRequest);
            List<UnlocationsResponse> unlocationsResponse = jsonHelper.convertValueToList(v1DataResponse.entities, UnlocationsResponse.class);
            if (unlocationsResponse != null && unlocationsResponse.size() > 0) {
                Map<String, String> locationMap = new HashMap<>();
                for (UnlocationsResponse unlocation : unlocationsResponse) {
                    locationMap.put(unlocation.getLocCode(), unlocation.getName());
                }

                for (IRunnerResponse response : responseList) {
                    if (((ShipmentListResponse) response).getCarrierDetails() != null) {
                        if (StringUtility.isNotEmpty(((ShipmentListResponse) response).getCarrierDetails().getOriginPort())) {
                            ((ShipmentListResponse) response).getCarrierDetails().setOriginPortName(locationMap.get(((ShipmentListResponse) response).getCarrierDetails().getOriginPort()));
                        }
                        if (StringUtility.isNotEmpty(((ShipmentListResponse) response).getCarrierDetails().getDestinationPort())) {
                            ((ShipmentListResponse) response).getCarrierDetails().setDestinationPortName(locationMap.get(((ShipmentListResponse) response).getCarrierDetails().getDestinationPort()));
                        }
                    }
                }
            }
        }
    }

    private void setContainerTeu(List<ShipmentDetails> shipmentDetails, List<IRunnerResponse> responseList) {

        Map<Long, ShipmentListResponse> dataMap = new HashMap<>();
        for (IRunnerResponse response : responseList){
            dataMap.put(((ShipmentListResponse)response).getId(), (ShipmentListResponse)response);
        }

        Set<String> containerType = new HashSet<>();

        for(ShipmentDetails shipment : shipmentDetails) {
            if(shipment.getContainersList() != null) {
                for(Containers containers : shipment.getContainersList()) {
                    if(StringUtility.isNotEmpty(containers.getContainerCode())) {
                        containerType.add(containers.getContainerCode());
                    }
                }
            }
        }

        if(containerType.size() > 0) {
            List<Object> criteria = Arrays.asList(
                    Arrays.asList("Code"),
                    "In",
                    Arrays.asList(containerType)
            );
            CommonV1ListRequest commonV1ListRequest = CommonV1ListRequest.builder().skip(0).take(0).criteriaRequests(criteria).build();
            V1DataResponse v1DataResponse = v1Service.fetchContainerTypeData(commonV1ListRequest);
            List<V1ContainerTypeResponse> containerTypeResponses = jsonHelper.convertValueToList(v1DataResponse.entities, V1ContainerTypeResponse.class);
            if(containerTypeResponses != null && containerTypeResponses.size() > 0) {
                Map<String, BigDecimal> containerTypeMap = new HashMap<>();
                for(V1ContainerTypeResponse containerTypeResponse : containerTypeResponses) {
                    if(containerTypeResponse.getTeu() != null) {
                        containerTypeMap.put(containerTypeResponse.getCode(), containerTypeResponse.getTeu());
                    }
                }

                BigDecimal teu;
                for(ShipmentDetails shipment : shipmentDetails) {
                    teu = BigDecimal.ZERO;
                    if(shipment.getContainersList() != null) {
                        for(Containers containers : shipment.getContainersList()) {
                            if(StringUtility.isNotEmpty(containers.getContainerCode())) {
                                if(containerTypeMap.containsKey(containers.getContainerCode()) && containers.getContainerCount() != null) {
                                    teu = teu.add(containerTypeMap.get(containers.getContainerCode()).multiply(BigDecimal.valueOf(containers.getContainerCount())));
                                }
                            }
                        }
                    }
                    dataMap.get(shipment.getId()).setTeuCount(teu);
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

    @Override
    @Transactional
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) {
        //ExecutorService executorService = Executors.newFixedThreadPool(100);

        ShipmentRequest request = (ShipmentRequest) commonRequestModel.getData();
        if (request == null) {
            log.error("Request is null for Shipment Create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        System.out.println(jsonHelper.convertToJson(request));

        if (request.getTransportMode().equals("AIR") && request.getShipmentType().equals("DRT"))
            directShipmentMAWBCheck(request);

        ShipmentDetails shipmentDetails = jsonHelper.convertCreateValue(request, ShipmentDetails.class);
        AdditionalDetails additionalDetails = jsonHelper.convertCreateValue(request.getAdditionalDetails(), AdditionalDetails.class);
        CarrierDetails carrierDetails = jsonHelper.convertCreateValue(request.getCarrierDetails(), CarrierDetails.class);

        try {

            if (request.getConsolidationList() != null) {
                List<ConsolidationDetailsRequest> consolRequest = request.getConsolidationList();
                List<ConsolidationDetails> consolList = consolidationDetailsDao.saveAll(commonUtils.convertToCreateEntityList(consolRequest, ConsolidationDetails.class));
                shipmentDetails.setConsolidationList(consolList);
            }

            if (request.getContainersList() != null) {
                List<ContainerRequest> containerRequest = request.getContainersList();
                List<Containers> containers = containerDao.saveAll(commonUtils.convertToCreateEntityList(containerRequest, Containers.class));
                shipmentDetails.setContainersList(containers);
            }

            getShipment(shipmentDetails);
            Long shipmentId = shipmentDetails.getId();

            List<PackingRequest> packingRequest = request.getPackingList();
            if (packingRequest != null)
                shipmentDetails.setPackingList(packingDao.saveEntityFromShipment(commonUtils.convertToCreateEntityList(packingRequest, Packing.class), shipmentId));

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

            try {
                shipmentSync.sync(shipmentDetails);
            } catch (Exception e){
                log.error("Error performing sync on shipment entity, {}", e);
            }
//            EventMessage eventMessage = EventMessage.builder().messageType(Constants.SERVICE).entity(Constants.SHIPMENT).request(shipmentDetails).build();
//            sbUtils.sendMessagesToTopic(isbProperties, azureServiceBusTopic.getTopic(), Arrays.asList(new ServiceBusMessage(jsonHelper.convertToJsonIncludeNulls(eventMessage))));

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
            TransactionAspectSupport.currentTransactionStatus().setRollbackOnly();
            throw new RuntimeException(e);
        }


//        CompletableFuture.allOf(createCallToAdditionalDetails, createCallToContainers, createCallToPackings, createCallToBookingCarriages, createCallToElDetails, createCallToEvents, createCallToFileRepos, createCallToJobs, createCallToNotes, createCallToReferenceNumbers, createCallToRoutings, createCallToServiceDetails, createCallToPickupDelivery, createCallToParties, createCallToCarrierDetails).join();
//        executorService.shutdownNow();
        return ResponseHelper.buildSuccessResponse(jsonHelper.convertValue(shipmentDetails, ShipmentDetailsResponse.class));
    }

    void getShipment(ShipmentDetails shipmentDetails) {
        shipmentDetails.setShipmentId(generateShipmentId());
        shipmentDetails = shipmentDao.save(shipmentDetails);
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

    @Override
    @Transactional
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) {
        String responseMsg;
        ShipmentRequest request = (ShipmentRequest) commonRequestModel.getData();
        if (request == null) {
            log.error("Request is empty for Shipment update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        if (request.getId() == null) {
            log.error("Request Id is null for Shipment update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        // TODO- implement Validation logic
        long id = request.getId();
        Optional<ShipmentDetails> oldEntity = shipmentDao.findById(id);
        if (!oldEntity.isPresent()) {
            log.debug("Shipment Details is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        ShipmentDetails entity = objectMapper.convertValue(request, ShipmentDetails.class);
        entity.setId(oldEntity.get().getId());
        if (entity.getContainersList() == null)
            entity.setContainersList(oldEntity.get().getContainersList());
        entity = shipmentDao.update(entity);
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
            ShipmentDetails entity = shipmentDao.save(shipmentDetails);
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
        CarrierDetailRequest carrierDetailRequest = shipmentRequest.getCarrierDetails();

        // TODO- implement Validation logic
        long id = shipmentRequest.getId();
        Optional<ShipmentDetails> oldEntity = shipmentDao.findById(id);
        if (!oldEntity.isPresent()) {
            log.debug("Shipment Details is null for Id {}", shipmentRequest.getId());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        List<Long> tempConsolIds = new ArrayList<>();

        List<ConsolidationDetailsRequest> updatedConsolidationRequest = new ArrayList<>();
        List<ConsolidationDetailsRequest> consolidationDetailsRequests = shipmentRequest.getConsolidationList();
        if (consolidationDetailsRequests != null && !consolidationDetailsRequests.isEmpty()) {
            for (ConsolidationDetailsRequest consolidation : consolidationDetailsRequests) {
                if (consolidation.getId() != null) {
                    tempConsolIds.add(consolidation.getId());
                }
            }
        }
        shipmentRequest.setConsolidationList(updatedConsolidationRequest);

        try {
            ShipmentDetails entity = objectMapper.convertValue(shipmentRequest, ShipmentDetails.class);
            entity.setId(oldEntity.get().getId());
            List<Containers> updatedContainers = null;
            if (containerRequestList != null) {
                updatedContainers = containerDao.updateEntityFromShipmentConsole(convertToEntityList(containerRequestList, Containers.class), null);
            } else {
                updatedContainers = oldEntity.get().getContainersList();
            }
            entity.setContainersList(updatedContainers);

            String oldEntityJsonString = jsonHelper.convertToJson(oldEntity.get());
            entity = shipmentDao.update(entity);

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(entity)
                            .prevData(jsonHelper.readFromJson(oldEntityJsonString, ShipmentDetails.class))
                            .parent(ShipmentDetails.class.getSimpleName())
                            .parentId(entity.getId())
                            .operation(DBOperationType.UPDATE.name()).build()
            );

            attachConsolidations(entity.getId(), tempConsolIds);

            ShipmentDetailsResponse response = shipmentDetailsMapper.map(entity);

            if (bookingCarriageRequestList != null) {
                List<BookingCarriage> updatedBookingCarriages = bookingCarriageDao.updateEntityFromShipment(convertToEntityList(bookingCarriageRequestList, BookingCarriage.class), id);
                response.setBookingCarriagesList(convertToDtoList(updatedBookingCarriages, BookingCarriageResponse.class));
            }
            if (packingRequestList != null) {
                List<Packing> updatedPackings = packingDao.updateEntityFromShipment(convertToEntityList(packingRequestList, Packing.class), id);
                response.setPackingList(convertToDtoList(updatedPackings, PackingResponse.class));
            }
            if (elDetailsRequestList != null) {
                List<ELDetails> updatedELDetails = elDetailsDao.updateEntityFromShipment(convertToEntityList(elDetailsRequestList, ELDetails.class), id);
                response.setElDetailsList(convertToDtoList(updatedELDetails, ELDetailsResponse.class));
            }
            if (eventsRequestList != null) {
                List<Events> updatedEvents = eventDao.updateEntityFromOtherEntity(convertToEntityList(eventsRequestList, Events.class), id, Constants.SHIPMENT);
                response.setEventsList(convertToDtoList(updatedEvents, EventsResponse.class));
            }
            if (jobRequestList != null) {
                List<Jobs> updatedJobs = jobDao.updateEntityFromShipment(convertToEntityList(jobRequestList, Jobs.class), id);
                response.setJobsList(convertToDtoList(updatedJobs, JobResponse.class));
            }
            if (referenceNumbersRequestList != null) {
                List<ReferenceNumbers> updatedReferenceNumbers = referenceNumbersDao.updateEntityFromShipment(convertToEntityList(referenceNumbersRequestList, ReferenceNumbers.class), id);
                response.setReferenceNumbersList(convertToDtoList(updatedReferenceNumbers, ReferenceNumbersResponse.class));
            }
            if (routingsRequestList != null) {
                List<Routings> updatedRoutings = routingsDao.updateEntityFromShipment(convertToEntityList(routingsRequestList, Routings.class), id);
                response.setRoutingsList(convertToDtoList(updatedRoutings, RoutingsResponse.class));
            }
            if (serviceDetailsRequestList != null) {
                List<ServiceDetails> updatedServiceDetails = serviceDetailsDao.updateEntityFromShipment(convertToEntityList(serviceDetailsRequestList, ServiceDetails.class), id);
                response.setServicesList(convertToDtoList(updatedServiceDetails, ServiceDetailsResponse.class));
            }
            if (fileRepoRequestList != null) {
                List<FileRepo> updatedFileRepos = fileRepoDao.updateEntityFromOtherEntity(convertToEntityList(fileRepoRequestList, FileRepo.class), id, Constants.SHIPMENT);
                response.setFileRepoList(convertToDtoList(updatedFileRepos, FileRepoResponse.class));
            }
            if (notesRequestList != null) {
                List<Notes> updatedNotes = notesDao.updateEntityFromOtherEntity(convertToEntityList(notesRequestList, Notes.class), id, Constants.SHIPMENT);
                response.setNotesList(convertToDtoList(updatedNotes, NotesResponse.class));
            }

            return ResponseHelper.buildSuccessResponse(response);
        } catch (ExecutionException e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            TransactionAspectSupport.currentTransactionStatus().setRollbackOnly();
            throw new RuntimeException(e);
        }
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
            log.info("Shipment details fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            ShipmentDetailsResponse response = shipmentDetailsMapper.map(shipmentDetails.get());
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
            // create common list request for shipment id


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
        if (shipmentRequest.getId() == null) {
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
        CarrierDetailRequest carrierDetailRequest = shipmentRequest.getCarrierDetails();
        // TODO- implement Validation logic
        long id = shipmentRequest.getId().get();
        Optional<ShipmentDetails> oldEntity = shipmentDao.findById(id);
        if (!oldEntity.isPresent()) {
            log.debug("Shipment Details is null for Id {}", shipmentRequest.getId());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        try {
            ShipmentDetails entity = oldEntity.get();
            shipmentDetailsMapper.update(shipmentRequest, entity);
            entity.setId(oldEntity.get().getId());
            List<Containers> updatedContainers = null;
            if (containerRequestList != null) {
                updatedContainers = containerDao.updateEntityFromShipmentConsole(convertToEntityList(containerRequestList, Containers.class), null);
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
                updatedCarrierDetails = carrierDao.updateEntityFromShipmentConsole(convertToClass(carrierDetailRequest, CarrierDetails.class));
                entity.setCarrierDetails(updatedCarrierDetails);
            }
            entity = shipmentDao.update(entity);

            ShipmentDetailsResponse response = shipmentDetailsMapper.map(entity);
            response.setContainersList(updatedContainers.stream().map(e -> objectMapper.convertValue(e, ContainerResponse.class)).collect(Collectors.toList()));
            if (additionalDetailRequest != null) {
                response.setAdditionalDetails(convertToClass(updatedAdditionalDetails, AdditionalDetailResponse.class));
            }
            if (carrierDetailRequest != null) {
                response.setCarrierDetails(convertToClass(updatedCarrierDetails, CarrierDetailResponse.class));
            }
            if (bookingCarriageRequestList != null) {
                List<BookingCarriage> updatedBookingCarriages = bookingCarriageDao.updateEntityFromShipment(convertToEntityList(bookingCarriageRequestList, BookingCarriage.class), id);
                response.setBookingCarriagesList(convertToDtoList(updatedBookingCarriages, BookingCarriageResponse.class));
            }
            if (packingRequestList != null) {
                List<Packing> updatedPackings = packingDao.updateEntityFromShipment(convertToEntityList(packingRequestList, Packing.class), id);
                response.setPackingList(convertToDtoList(updatedPackings, PackingResponse.class));
            }
            if (elDetailsRequestList != null) {
                List<ELDetails> updatedELDetails = elDetailsDao.updateEntityFromShipment(convertToEntityList(elDetailsRequestList, ELDetails.class), id);
                response.setElDetailsList(convertToDtoList(updatedELDetails, ELDetailsResponse.class));
            }
            if (eventsRequestList != null) {
                List<Events> updatedEvents = eventDao.updateEntityFromOtherEntity(convertToEntityList(eventsRequestList, Events.class), id, Constants.SHIPMENT);
                response.setEventsList(convertToDtoList(updatedEvents, EventsResponse.class));
            }
            if (fileRepoRequestList != null) {
                List<FileRepo> updatedFileRepos = fileRepoDao.updateEntityFromOtherEntity(convertToEntityList(fileRepoRequestList, FileRepo.class), id, Constants.SHIPMENT);
                response.setFileRepoList(convertToDtoList(updatedFileRepos, FileRepoResponse.class));
            }
            if (jobRequestList != null) {
                List<Jobs> updatedJobs = jobDao.updateEntityFromShipment(convertToEntityList(jobRequestList, Jobs.class), id);
                response.setJobsList(convertToDtoList(updatedJobs, JobResponse.class));
            }
            if (notesRequestList != null) {
                List<Notes> updatedNotes = notesDao.updateEntityFromOtherEntity(convertToEntityList(notesRequestList, Notes.class), id, Constants.SHIPMENT);
                response.setNotesList(convertToDtoList(updatedNotes, NotesResponse.class));
            }
            if (referenceNumbersRequestList != null) {
                List<ReferenceNumbers> updatedReferenceNumbers = referenceNumbersDao.updateEntityFromShipment(convertToEntityList(referenceNumbersRequestList, ReferenceNumbers.class), id);
                response.setReferenceNumbersList(convertToDtoList(updatedReferenceNumbers, ReferenceNumbersResponse.class));
            }
            if (routingsRequestList != null) {
                List<Routings> updatedRoutings = routingsDao.updateEntityFromShipment(convertToEntityList(routingsRequestList, Routings.class), id);
                response.setRoutingsList(convertToDtoList(updatedRoutings, RoutingsResponse.class));
            }
            if (serviceDetailsRequestList != null) {
                List<ServiceDetails> updatedServiceDetails = serviceDetailsDao.updateEntityFromShipment(convertToEntityList(serviceDetailsRequestList, ServiceDetails.class), id);
                response.setServicesList(convertToDtoList(updatedServiceDetails, ServiceDetailsResponse.class));
            }

            return ResponseHelper.buildSuccessResponse(response);
        } catch (ExecutionException e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public ResponseEntity<?> toggleLock(CommonRequestModel commonRequestModel) {
        CommonGetRequest commonGetRequest = (CommonGetRequest) commonRequestModel.getData();
        Long id = commonGetRequest.getId();
        ShipmentDetails shipmentDetails = shipmentDao.findById(id).get();
        String lockingUser = shipmentDetails.getLockedBy();
        String currentUser = userContext.getUser().getUsername();

        if (shipmentDetails.getIsLocked()) {
            if (lockingUser != null && lockingUser.equals(currentUser))
                shipmentDetails.setIsLocked(false);
        } else {
            shipmentDetails.setIsLocked(true);
            shipmentDetails.setLockedBy(currentUser);
        }
        shipmentDao.save(shipmentDetails);

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
        if (shipmentSettingsList.isEmpty())
            return StringUtility.getRandomString(10);
        return createShipmentSequence(shipmentSettingsList.get(0));
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

    private V1DataResponse fetchCarrierDetailsFromV1(String mawbAirlineCode) {
        CommonV1ListRequest request = new CommonV1ListRequest();
        List<Object> criteria = new ArrayList<>();
        criteria.addAll(List.of(List.of("AirlineCode"), "=", mawbAirlineCode));
        request.setCriteriaRequests(criteria);
        CarrierListObject carrierListObject = new CarrierListObject();
        carrierListObject.setListObject(request);
        V1DataResponse response = v1Service.fetchCarrierMasterData(carrierListObject, true);
        return response;
    }

    private void directShipmentMAWBCheck(ShipmentRequest shipmentRequest) {
        if (StringUtility.isEmpty(shipmentRequest.getMasterBill())) {
            return;
        }

        if (!isMAWBNumberValid(shipmentRequest.getMasterBill()))
            throw new ValidationException("Please enter a valid MAWB number.");

        String mawbAirlineCode = shipmentRequest.getMasterBill().substring(0, 3);

        V1DataResponse v1DataResponse = fetchCarrierDetailsFromV1(mawbAirlineCode);
        List<CarrierResponse> carrierDetails = jsonHelper.convertValueToList(v1DataResponse.entities, CarrierResponse.class);
        if (carrierDetails == null || carrierDetails.size()==0)
                throw new ValidationException("Airline for the entered MAWB Number doesn't exist in Carrier Master");


        CarrierResponse correspondingCarrier = carrierDetails.get(0); //carrierDetails.getContent().get(0);

        Boolean isMAWBNumberExist = false;
        Boolean isCarrierExist = false;
        if (shipmentRequest.getCarrierDetails() != null)
            isCarrierExist = true;

        if (isCarrierExist && !shipmentRequest.getCarrierDetails().getShippingLine().equals(correspondingCarrier.getItemValue()))
            throw new ValidationException("MAWB Number prefix is not matching with entered Flight Carrier");

        ListCommonRequest listMawbRequest = constructListCommonRequest("mawbNumber", shipmentRequest.getMasterBill(), "=");
        Pair<Specification<MawbStocksLink>, Pageable> mawbStocksLinkPair = fetchData(listMawbRequest, MawbStocksLink.class);
        Page<MawbStocksLink> mawbStocksLinkPage = mawbStocksLinkDao.findAll(mawbStocksLinkPair.getLeft(), mawbStocksLinkPair.getRight());

        MawbStocksLink mawbStocksLink = null;

        if (mawbStocksLinkPage.getContent() != null && mawbStocksLinkPage.getTotalElements() > 0) {
            isMAWBNumberExist = true;
            mawbStocksLink = mawbStocksLinkPage.getContent().get(0);
        }

        if (!isCarrierExist)
            shipmentRequest.setCarrierDetails(jsonHelper.convertValue(correspondingCarrier, CarrierDetailRequest.class));

        if (shipmentRequest.getDirection() == "IMP") {
            return;
        }

        if (isMAWBNumberExist) {
            if (mawbStocksLink.getStatus() == "Consumed" && mawbStocksLink.getEntityId() != shipmentRequest.getId()) // If MasterBill number is already Consumed.
                throw new ValidationException("The MAWB number entered is already consumed. Please enter another MAWB number.");
        } else {
            createNewMAWBEntry(shipmentRequest);
        }
    }

    private void createNewMAWBEntry(ShipmentRequest shipmentRequest) {
        MawbStocks mawbStocks = new MawbStocks();
        mawbStocks.setAirLinePrefix(shipmentRequest.getCarrierDetails().getShippingLine());
        mawbStocks.setCount("1");
        mawbStocks.setStartNumber(Long.valueOf(shipmentRequest.getMasterBill().substring(4, 10)));
        mawbStocks.setFrom(shipmentRequest.getMasterBill());
        mawbStocks.setTo(shipmentRequest.getMasterBill());
        mawbStocks.setMawbNumber(shipmentRequest.getMasterBill());
        mawbStocks.setStatus("Unused");
        // if(shipmentRequest.getBorrowedFrom()!=null) mawbStocks.setBorrowedFrom(Long.valueOf(shipmentRequest.getBorrowedFrom())); TODO fetch from v1
        mawbStocks.setHomePort(shipmentRequest.getCarrierDetails().getOriginPort());
        mawbStocks = mawbStocksDao.save(mawbStocks);

        if (mawbStocks.getId() != null) {
            var entryForMawbStocksLinkRow = new MawbStocksLink();
            entryForMawbStocksLinkRow.setParentId(mawbStocks.getId());
            entryForMawbStocksLinkRow.setSeqNumber(shipmentRequest.getMasterBill().substring(4, 10));
            entryForMawbStocksLinkRow.setMawbNumber(shipmentRequest.getMasterBill());
            entryForMawbStocksLinkRow.setStatus("Unused");
            entryForMawbStocksLinkRow = mawbStocksLinkDao.save(entryForMawbStocksLinkRow);
        }
    }

    private Boolean isMAWBNumberValid(String masterBill) {
        Boolean MAWBNumberValidity = true;
        if (masterBill.length() == 12) {
            String mawbSeqNum = masterBill.substring(4, 11);
            String checkDigit = masterBill.substring(11, 12);
            Long imawbSeqNum = 0L;
            Long icheckDigit = 0L;
            if (areAllCharactersDigits(masterBill, 4, 12)) { // masterBill.substring(4, 12).matches("\\d+")
                imawbSeqNum = Long.valueOf(mawbSeqNum);
                icheckDigit = Long.valueOf(checkDigit);
                if (imawbSeqNum % 7 != icheckDigit)
                    MAWBNumberValidity = false;
            } else MAWBNumberValidity = false;
        } else MAWBNumberValidity = false;
        return MAWBNumberValidity;
    }

    private boolean areAllCharactersDigits(String input, int startIndex, int endIndex) {
        String substring = input.substring(startIndex, endIndex);
        for (int i = 0; i < substring.length(); i++) {
            if (!Character.isDigit(substring.charAt(i))) {
                return false;
            }
        }
        return true;
    }

    @Transactional
    public ResponseEntity<?> completeV1ShipmentCreateAndUpdate(CommonRequestModel commonRequestModel) throws Exception {

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

        List<Long> tempConsolIds = new ArrayList<>();

        List<ConsolidationDetailsRequest> updatedConsolidationRequest = new ArrayList<>();
        List<ConsolidationDetailsRequest> consolidationDetailsRequests = shipmentRequest.getConsolidationList();
        if(consolidationDetailsRequests != null && !consolidationDetailsRequests.isEmpty()) {
            for(ConsolidationDetailsRequest consolidation : consolidationDetailsRequests) {
                if(consolidation.getId() != null) {
                    tempConsolIds.add(consolidation.getId());
                }
            }
        }
        shipmentRequest.setConsolidationList(updatedConsolidationRequest);

        try {
            List<Containers> oldContainers = null;
            Long id = null;
            ShipmentDetails oldShipment = null;
            if(oldEntity != null && oldEntity.isPresent()) {
                oldShipment = oldEntity.get();
                id = oldEntity.get().getId();
                oldContainers = oldEntity.get().getContainersList();
            }
            ShipmentDetails entity = objectMapper.convertValue(shipmentRequest, ShipmentDetails.class);
            entity.setId(id);
            List<Containers> updatedContainers = null;
            if (containerRequestList != null) {
                updatedContainers = containerDao.updateEntityFromShipmentConsole(convertToEntityList(containerRequestList, Containers.class), null, oldContainers);
            } else if(oldEntity != null && !oldEntity.isEmpty()){
                updatedContainers = oldEntity.get().getContainersList();
            }
            entity.setContainersList(updatedContainers);

            if(id == null) {
                entity = shipmentDao.save(entity);
                id = entity.getId();
            } else {
                entity = shipmentDao.update(entity);
            }

            attachConsolidations(entity.getId(), tempConsolIds);

            ShipmentDetailsResponse response = shipmentDetailsMapper.map(entity);

            if (bookingCarriageRequestList != null) {
                ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentId", entity.getId(), "=");
                Pair<Specification<BookingCarriage>, Pageable> bookingCarriagePair = fetchData(listCommonRequest, BookingCarriage.class);
                Page<BookingCarriage> oldBookingCarriages = bookingCarriageDao.findAll(bookingCarriagePair.getLeft(), bookingCarriagePair.getRight());
                List<BookingCarriage> updatedBookingCarriages = bookingCarriageDao.updateEntityFromShipment(convertToEntityList(bookingCarriageRequestList, BookingCarriage.class), id, oldBookingCarriages.stream().toList());
                response.setBookingCarriagesList(convertToDtoList(updatedBookingCarriages, BookingCarriageResponse.class));
            }
            if (packingRequestList != null) {
                ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentId", entity.getId(), "=");
                Pair<Specification<Packing>, Pageable> packingPair = fetchData(listCommonRequest, Packing.class);
                Page<Packing> oldPackings = packingDao.findAll(packingPair.getLeft(), packingPair.getRight());
                List<Packing> updatedPackings = packingDao.updateEntityFromShipment(convertToEntityList(packingRequestList, Packing.class), id, oldPackings.stream().toList());
                response.setPackingList(convertToDtoList(updatedPackings, PackingResponse.class));
            }
            if (elDetailsRequestList != null) {
                ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentId", entity.getId(), "=");
                Pair<Specification<ELDetails>, Pageable> elDetailsPair = fetchData(listCommonRequest, ELDetails.class);
                Page<ELDetails> oldELDetails = elDetailsDao.findAll(elDetailsPair.getLeft(), elDetailsPair.getRight());
                List<ELDetails> updatedELDetails = elDetailsDao.updateEntityFromShipment(convertToEntityList(elDetailsRequestList, ELDetails.class), id, oldELDetails.stream().toList());
                response.setElDetailsList(convertToDtoList(updatedELDetails, ELDetailsResponse.class));
            }
            if (eventsRequestList != null) {
                ListCommonRequest listCommonRequest = constructListRequestFromEntityId(entity.getId(), Constants.SHIPMENT);
                Pair<Specification<Events>, Pageable> pair = fetchData(listCommonRequest, Events.class);
                Page<Events> oldEvents = eventDao.findAll(pair.getLeft(), pair.getRight());
                List<Events> updatedEvents = eventDao.updateEntityFromOtherEntity(convertToEntityList(eventsRequestList, Events.class), id, Constants.SHIPMENT, oldEvents.stream().toList());
                response.setEventsList(convertToDtoList(updatedEvents, EventsResponse.class));
            }
            if (jobRequestList != null) {
                ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentId", entity.getId(), "=");
                Pair<Specification<Jobs>, Pageable> pair = fetchData(listCommonRequest, Jobs.class);
                Page<Jobs> oldJobs = jobDao.findAll(pair.getLeft(), pair.getRight());
                List<Jobs> updatedJobs = jobDao.updateEntityFromShipment(convertToEntityList(jobRequestList, Jobs.class), id, oldJobs.stream().toList());
                response.setJobsList(convertToDtoList(updatedJobs, JobResponse.class));
            }
            if (referenceNumbersRequestList != null) {
                ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentId", entity.getId(), "=");
                Pair<Specification<ReferenceNumbers>, Pageable> pair = fetchData(listCommonRequest, ReferenceNumbers.class);
                Page<ReferenceNumbers> oldReferenceNumbers = referenceNumbersDao.findAll(pair.getLeft(), pair.getRight());
                List<ReferenceNumbers> updatedReferenceNumbers = referenceNumbersDao.updateEntityFromShipment(convertToEntityList(referenceNumbersRequestList, ReferenceNumbers.class), id, oldReferenceNumbers.stream().toList());
                response.setReferenceNumbersList(convertToDtoList(updatedReferenceNumbers, ReferenceNumbersResponse.class));
            }
            if (routingsRequestList != null) {
                ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentId", entity.getId(), "=");
                Pair<Specification<Routings>, Pageable> pair = fetchData(listCommonRequest, Routings.class);
                Page<Routings> oldRoutings = routingsDao.findAll(pair.getLeft(), pair.getRight());
                List<Routings> updatedRoutings = routingsDao.updateEntityFromShipment(convertToEntityList(routingsRequestList, Routings.class), id, oldRoutings.stream().toList());
                response.setRoutingsList(convertToDtoList(updatedRoutings, RoutingsResponse.class));
            }
            if (serviceDetailsRequestList != null) {
                ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentId", entity.getId(), "=");
                Pair<Specification<ServiceDetails>, Pageable> pair = fetchData(listCommonRequest, ServiceDetails.class);
                Page<ServiceDetails> oldServiceDetails = serviceDetailsDao.findAll(pair.getLeft(), pair.getRight());
                List<ServiceDetails> updatedServiceDetails = serviceDetailsDao.updateEntityFromShipment(convertToEntityList(serviceDetailsRequestList, ServiceDetails.class), id, oldServiceDetails.stream().toList());
                response.setServicesList(convertToDtoList(updatedServiceDetails, ServiceDetailsResponse.class));
            }
            if (fileRepoRequestList != null) {
                ListCommonRequest listCommonRequest = constructListRequestFromEntityId(entity.getId(), Constants.SHIPMENT);
                Pair<Specification<FileRepo>, Pageable> pair = fetchData(listCommonRequest, FileRepo.class);
                Page<FileRepo> oldFileRepoList = fileRepoDao.findAll(pair.getLeft(), pair.getRight());
                List<FileRepo> updatedFileRepos = fileRepoDao.updateEntityFromOtherEntity(convertToEntityList(fileRepoRequestList, FileRepo.class), id, Constants.SHIPMENT, oldFileRepoList.stream().toList());
                response.setFileRepoList(convertToDtoList(updatedFileRepos, FileRepoResponse.class));
            }
            if (notesRequestList != null) {
                ListCommonRequest listCommonRequest = constructListRequestFromEntityId(entity.getId(), Constants.SHIPMENT);
                Pair<Specification<Notes>, Pageable> pair = fetchData(listCommonRequest, Notes.class);
                Page<Notes> oldNoteList = notesDao.findAll(pair.getLeft(), pair.getRight());
                List<Notes> updatedNotes = notesDao.updateEntityFromOtherEntity(convertToEntityList(notesRequestList, Notes.class), id, Constants.SHIPMENT, oldNoteList.stream().toList());
                response.setNotesList(convertToDtoList(updatedNotes, NotesResponse.class));
            }

            return ResponseHelper.buildSuccessResponse(response);
        } catch (ExecutionException e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }


    private void createShipmentPayload (ShipmentDetails shipmentDetails, ShipmentDetailsResponse shipmentDetailsResponse) {
        this.addAllMasterDatas(shipmentDetails, shipmentDetailsResponse);
        this.addAllUnlocationDatas(shipmentDetails, shipmentDetailsResponse);
        this.addDedicatedMasterData(shipmentDetails, shipmentDetailsResponse);
        this.addAllContainerTypesInSingleCall(shipmentDetails,shipmentDetailsResponse);
    }
    private void addAllMasterDatas (ShipmentDetails shipmentDetails, ShipmentDetailsResponse shipmentDetailsResponse) {
        shipmentDetailsResponse.setMasterData(masterDataUtils.addMasterData(shipmentDetailsResponse, ShipmentDetails.class));
        if(shipmentDetailsResponse.getAdditionalDetails() != null) {
            shipmentDetailsResponse.getAdditionalDetails().setMasterData(masterDataUtils.addMasterData(shipmentDetailsResponse.getAdditionalDetails(), AdditionalDetails.class));
        }
        if(shipmentDetailsResponse.getCarrierDetails() != null) {
            shipmentDetailsResponse.getCarrierDetails().setMasterData(masterDataUtils.addMasterData(shipmentDetailsResponse.getCarrierDetails(), CarrierDetails.class));
        }
    }



    private void addAllUnlocationDatas (ShipmentDetails shipmentDetails, ShipmentDetailsResponse shipmentDetailsResponse) {
        if(shipmentDetailsResponse.getAdditionalDetails() != null) {
            shipmentDetailsResponse.getAdditionalDetails().setUnlocationData(masterDataUtils.addUnlocationData(shipmentDetailsResponse.getAdditionalDetails(), AdditionalDetails.class, EntityTransferConstants.UNLOCATION_CODE));
        }
        if(shipmentDetailsResponse.getCarrierDetails() != null) {
            shipmentDetailsResponse.getCarrierDetails().setUnlocationData(masterDataUtils.addUnlocationData(shipmentDetailsResponse.getCarrierDetails(), CarrierDetails.class, EntityTransferConstants.UNLOCATION_CODE));
        }
    }



    private void addDedicatedMasterData (ShipmentDetails shipmentDetails, ShipmentDetailsResponse shipmentDetailsResponse) {
        if(shipmentDetailsResponse.getCarrierDetails() != null) {
            shipmentDetailsResponse.getCarrierDetails().setCarrierMasterData(masterDataUtils.carrierMasterData(shipmentDetailsResponse.getCarrierDetails(), CarrierDetails.class));
        }
        shipmentDetailsResponse.setCurrenciesMasterData(masterDataUtils.currencyMasterData(shipmentDetails, ShipmentDetails.class));
    }

    private void addAllContainerTypesInSingleCall(ShipmentDetails shipmentDetails, ShipmentDetailsResponse shipmentDetailsResponse) {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        List<String> containerTypes = new ArrayList<>();
        if (!Objects.isNull(shipmentDetailsResponse.getContainersList()))
            shipmentDetailsResponse.getContainersList().forEach(r -> containerTypes.addAll(masterDataUtils.createInBulkCommodityTypeRequest(r, Containers.class, fieldNameKeyMap, String.valueOf(r.hashCode()))));

        Map<String, EntityTransferCommodityType> v1Data = masterDataUtils.fetchInBulkCommodityTypes(containerTypes);

        if (!Objects.isNull(shipmentDetailsResponse.getContainersList()))
            shipmentDetailsResponse.getContainersList().forEach(r -> r.setCommodityTypeData(masterDataUtils.setInBulkCommodityTypes(fieldNameKeyMap.get(String.valueOf(r.hashCode())), v1Data)));
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
            cloneShipmentDetails.setContainersList(null);

            CommonRequestModel requestModel = CommonRequestModel.buildRequest(cloneShipmentDetails);
            log.info("Shipment details cloning started for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            return this.create(requestModel);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

}
