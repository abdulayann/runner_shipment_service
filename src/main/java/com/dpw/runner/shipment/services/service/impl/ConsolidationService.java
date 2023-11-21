package com.dpw.runner.shipment.services.service.impl;


import com.dpw.runner.shipment.services.Kafka.Dto.KafkaResponse;
import com.dpw.runner.shipment.services.Kafka.Producer.KafkaProducer;
import com.dpw.runner.shipment.services.adapters.interfaces.ITrackingServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.CacheConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.*;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.VolumeWeightChargeable;
import com.dpw.runner.shipment.services.dto.TrackingService.UniversalTrackingPayload;
import com.dpw.runner.shipment.services.dto.patchRequest.ConsolidationPatchRequest;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.response.ConsolidationDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.ConsolidationListResponse;
import com.dpw.runner.shipment.services.dto.v1.request.ConsoleBookingListRequest;
import com.dpw.runner.shipment.services.dto.v1.response.ConsoleBookingListResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.IntegrationType;
import com.dpw.runner.shipment.services.entity.enums.ProductProcessTypes;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCommodityType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferMasterLists;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferUnLocations;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.mapper.ConsolidationDetailsMapper;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequest;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationService;
import com.dpw.runner.shipment.services.service.interfaces.IContainerService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service_bus.AzureServiceBusTopic;
import com.dpw.runner.shipment.services.service_bus.ISBProperties;
import com.dpw.runner.shipment.services.service_bus.SBUtilsImpl;
import com.dpw.runner.shipment.services.syncing.interfaces.IConsolidationSync;
import com.dpw.runner.shipment.services.utils.*;
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
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.interceptor.TransactionAspectSupport;

import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.OutputStream;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.*;
import static com.dpw.runner.shipment.services.utils.UnitConversionUtility.convertUnit;

@SuppressWarnings("ALL")
@Service
@Slf4j
public class ConsolidationService implements IConsolidationService {

    @Autowired
    private IConsolidationDetailsDao consolidationDetailsDao;
    @Autowired
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;
    @Autowired
    private ICarrierDao carrierDao;
    @Autowired
    private IAllocationsDao allocationsDao;
    @Autowired
    private IAchievedQuantitiesDao achievedQuantitiesDao;
    @Autowired
    private IPartiesDao partiesDao;

    private final CSVParsingUtil<ConsolidationDetails> parser = new CSVParsingUtil<>(ConsolidationDetails.class);

    @Autowired
    private JsonHelper jsonHelper;
    @Autowired
    private ObjectMapper objectMapper;

    @Autowired
    private IPackingDao packingDao;

    @Autowired
    private IEventDao eventDao;

    @Autowired
    private IFileRepoDao fileRepoDao;

    @Autowired
    private IJobDao jobDao;

    @Autowired
    private INotesDao notesDao;

    @Autowired
    private IReferenceNumbersDao referenceNumbersDao;

    @Autowired
    private IRoutingsDao routingsDao;

    @Autowired
    private IContainerDao containerDao;

    @Autowired
    private IContainerService containerService;

    @Autowired
    private UserContext userContext;

    @Autowired
    private IShipmentDao shipmentDao;

    @Autowired
    private IShipmentSettingsDao shipmentSettingsDao;

    @Autowired
    private SBUtilsImpl sbUtils;

    @Autowired
    private ISBProperties isbProperties;

    @Autowired
    private AzureServiceBusTopic azureServiceBusTopic;

    @Autowired
    private IConsolidationSync consolidationSync;

    @Autowired
    private AuditLogService auditLogService;

    @Autowired
    private IShipmentsContainersMappingDao shipmentsContainersMappingDao;

    @Autowired
    private ModelMapper modelMapper;

    @Autowired
    private IV1Service v1Service;

    @Autowired
    private MasterDataUtils masterDataUtils;

    @Autowired
    private ConsolidationDetailsMapper consolidationDetailsMapper;

    @Autowired
    private ProductIdentifierUtility productEngine;

    @Autowired
    private ITrackingServiceAdapter trackingServiceAdapter;
    
    @Autowired
    private KafkaProducer producer;

    @Value("${consolidationsKafka.queue}")
    private String senderQueue;

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
            Map.entry("type", RunnerEntityMapping.builder().tableName("parties").dataType(String.class).build()),
            Map.entry("cutoffDate", RunnerEntityMapping.builder().tableName("allocations").dataType(LocalDateTime.class).build()),
            Map.entry("createdBy", RunnerEntityMapping.builder().tableName("ConsolidationDetails").dataType(String.class).build()),
//            Map.entry("bookingId", RunnerEntityMapping.builder().tableName("BookingCharges").dataType(Long.class).build()),
            Map.entry("orderNumber", RunnerEntityMapping.builder().tableName("Jobs").dataType(String.class).build()),
//            Map.entry("bookingStatus", RunnerEntityMapping.builder().tableName("CustomerBooking").dataType(BookingStatus.class).build()),
            Map.entry("orgCode", RunnerEntityMapping.builder().tableName("parties").dataType(Integer.class).build()),
            Map.entry("referenceNumber", RunnerEntityMapping.builder().tableName("ConsolidationDetails").dataType(String.class).build()),
            Map.entry("shipmentId", RunnerEntityMapping.builder().tableName("shipmentsList").dataType(String.class).build()),
            Map.entry("consolidationNumber", RunnerEntityMapping.builder().tableName("ConsolidationDetails").dataType(String.class).build()),
            Map.entry("consolidationType", RunnerEntityMapping.builder().tableName("ConsolidationDetails").dataType(String.class).build()),
            Map.entry("transportMode", RunnerEntityMapping.builder().tableName("ConsolidationDetails").dataType(String.class).build()),
            Map.entry("releaseType", RunnerEntityMapping.builder().tableName("ConsolidationDetails").dataType(String.class).build()),
            Map.entry("deliveryMode", RunnerEntityMapping.builder().tableName("ConsolidationDetails").dataType(String.class).build()),
            Map.entry("containerCategory", RunnerEntityMapping.builder().tableName("ConsolidationDetails").dataType(String.class).build()),
            Map.entry("shipmentType", RunnerEntityMapping.builder().tableName("ConsolidationDetails").dataType(String.class).build()),
            Map.entry("mawb", RunnerEntityMapping.builder().tableName("ConsolidationDetails").dataType(String.class).build()),
            Map.entry("serviceLevel", RunnerEntityMapping.builder().tableName("ConsolidationDetails").dataType(String.class).build()),
            Map.entry("bookingType", RunnerEntityMapping.builder().tableName("ConsolidationDetails").dataType(String.class).build()),
            Map.entry("addressCode", RunnerEntityMapping.builder().tableName("parties").dataType(Integer.class).build()),
            Map.entry("shippingLine", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(String.class).build()),
            Map.entry("vessel", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(String.class).build()),
            Map.entry("voyage", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(String.class).build()),
            Map.entry("origin", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(String.class).build()),
            Map.entry("destination", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(String.class).build()),
            Map.entry("eta", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(LocalDateTime.class).build()),
            Map.entry("etd", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(LocalDateTime.class).build()),
            Map.entry("ata", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(LocalDateTime.class).build()),
            Map.entry("containerNumber", RunnerEntityMapping.builder().tableName("containersList").dataType(String.class).build()),
            Map.entry("containerCode", RunnerEntityMapping.builder().tableName("containersList").dataType(String.class).build()),
            Map.entry("bookingCutoff", RunnerEntityMapping.builder().tableName("ConsolidationDetails").dataType(LocalDateTime.class).build()),
            Map.entry("estimatedTerminalCutoff", RunnerEntityMapping.builder().tableName("ConsolidationDetails").dataType(LocalDateTime.class).build()),
            Map.entry("hazardousBookingCutoff", RunnerEntityMapping.builder().tableName("ConsolidationDetails").dataType(LocalDateTime.class).build()),
            Map.entry("isDomestic", RunnerEntityMapping.builder().tableName("ConsolidationDetails").dataType(Boolean.class).build()),
            Map.entry("payment", RunnerEntityMapping.builder().tableName("ConsolidationDetails").dataType(String.class).build()),
            Map.entry("reeferCutoff", RunnerEntityMapping.builder().tableName("ConsolidationDetails").dataType(LocalDateTime.class).build()),
            Map.entry("shipInstructionCutoff", RunnerEntityMapping.builder().tableName("ConsolidationDetails").dataType(LocalDateTime.class).build()),
            Map.entry("terminalCutoff", RunnerEntityMapping.builder().tableName("ConsolidationDetails").dataType(LocalDateTime.class).build()),
            Map.entry("verifiedGrossMassCutoff", RunnerEntityMapping.builder().tableName("ConsolidationDetails").dataType(LocalDateTime.class).build()),
            Map.entry("guid", RunnerEntityMapping.builder().tableName("ConsolidationDetails").dataType(UUID.class).fieldName("guid").build())
            );

    @Override
    @Transactional
    public List<ConsolidationDetails> createTestConsolidations(Integer count) {
        List<ConsolidationDetails> response = new ArrayList<>();
        /**
         * * * * * * *
         * * * */

        for (int i = 0; i < count; i++) {

            ConsolidationDetails consolidationDetails = createConsolidationData();
            /**
             * Carrier Details*
             */
            CarrierDetails carrierDetail = createCarrier();
            consolidationDetails.setCarrierDetails(carrierDetail);

            consolidationDetails = consolidationDetailsDao.save(consolidationDetails, false);
            afterSave(consolidationDetails, true);
        }

        return response;
    }

    @Override
    public ResponseEntity<?> fetchConsolidations(CommonRequestModel commonRequestModel) {
        ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();

        Pair<Specification<ConsolidationDetails>, Pageable> tuple = fetchData(request, ConsolidationDetails.class, tableNames);
        Page<ConsolidationDetails> consolidationDetailsPage = consolidationDetailsDao.findAll(tuple.getLeft(), tuple.getRight());
        return ResponseHelper.buildListSuccessResponse(
                convertEntityListToDtoList(consolidationDetailsPage.getContent()),
                consolidationDetailsPage.getTotalPages(),
                consolidationDetailsPage.getTotalElements());
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<ConsolidationDetails> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        List<ConsolidationListResponse> consolidationListResponses = new ArrayList<>();
        lst.forEach(consolidationDetails -> {
            var res = (modelMapper.map(consolidationDetails, ConsolidationListResponse.class));
            updateHouseBillsShippingIds(consolidationDetails, res);
            consolidationListResponses.add(res);
        });
        if(consolidationListResponses != null && consolidationListResponses.size() > 0) {
            List<UUID> guidsList = new ArrayList<>();
            for (ConsolidationListResponse consolidationDetails: consolidationListResponses) {
                guidsList.add(consolidationDetails.getGuid());
            }
            if(guidsList.size() > 0) {
                ConsoleBookingListRequest consoleBookingListRequest = new ConsoleBookingListRequest();
                consoleBookingListRequest.setGuidsList(guidsList);
                try {
                    ConsoleBookingListResponse consoleBookingListResponse = v1Service.fetchConsolidationBookingData(consoleBookingListRequest);
                    if (consoleBookingListResponse.getData() != null && !consoleBookingListResponse.getData().isEmpty()) {
                        for (ConsolidationListResponse consolidationDetails : consolidationListResponses) {
                            if (consoleBookingListResponse.getData().get(consolidationDetails.getGuid()) != null) {
                                consolidationDetails.setBookingStatus(consoleBookingListResponse.getData().get(consolidationDetails.getGuid()).getStatus());
                                consolidationDetails.setBookingId(consoleBookingListResponse.getData().get(consolidationDetails.getGuid()).getIntraBookingId());
                            }
                        }
                    }
                } catch (Exception e) {
                    log.debug(e.getMessage());
                }
            }
        }
        consolidationListResponses.forEach(consolidationDetails -> {
            responseList.add(consolidationDetails);
        });
        return responseList;
    }

    private void updateHouseBillsShippingIds(ConsolidationDetails consol, ConsolidationListResponse consolidationRes) {
        var shipments = consol.getShipmentsList();
        List<String> shipmentIds = null;
        List<String> houseBills = null;
        if (shipments != null)
            shipmentIds = shipments.stream().map(shipment -> shipment.getShipmentId()).collect(Collectors.toList());
        if (shipmentIds != null)
            houseBills = shipments.stream().map(shipment -> shipment.getHouseBill()).filter(houseBill -> Objects.nonNull(houseBill)).collect(Collectors.toList());
        consolidationRes.setHouseBills(houseBills);
        consolidationRes.setShipmentIds(shipmentIds);
    }

    private List<Parties> createParties(ConsolidationDetails consolidationDetails) {
        List<Parties> parties = new ArrayList<>();
        int random = new Random().nextInt(100);
        for (String partyType : PARTY_TYPE) {
            Parties party = Parties.builder()
                    .type(partyType).orgCode(generateString(7)).addressCode(generateString(7))
                    .orgData(ORG).addressData(ADDRESS)
                    .entityId(consolidationDetails.getId()).entityType("CONSOLIDATION")
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

    //TODO - Fill more data in the consolidation
    private ConsolidationDetails createConsolidationData() {
        int random = new Random().nextInt(100);
        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder().transportMode(TRANSPORT_MODES.get(random % TRANSPORT_MODES.size())).shipmentType(SHIPMENT_TYPE.get(random % SHIPMENT_TYPE.size()))
                .build();
        consolidationDetails.setTenantId(1);
        return consolidationDetails;
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

        ConsolidationDetailsRequest request = (ConsolidationDetailsRequest) commonRequestModel.getData();
        if (request == null) {
            log.error("Request is null for Consolidation Create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

        System.out.println(jsonHelper.convertToJson(request));
        ConsolidationDetails consolidationDetails = jsonHelper.convertValue(request, ConsolidationDetails.class);
        CarrierDetails carrierDetails = jsonHelper.convertValue(request.getCarrierDetails(), CarrierDetails.class);
        Allocations allocations = jsonHelper.convertValue(request.getAllocations(), Allocations.class);
        AchievedQuantities achievedQuantities = jsonHelper.convertValue(request.getAchievedQuantities(), AchievedQuantities.class);

        try {
            consolidationDetails.setShipmentsList(null);

            getConsolidation(consolidationDetails);

            List<ShipmentSettingsDetails> shipmentSettingsDetailsList = shipmentSettingsDao.getSettingsByTenantIds(List.of(TenantContext.getCurrentTenant()));
            ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
            if(shipmentSettingsDetailsList.size() > 0)
                shipmentSettingsDetails = shipmentSettingsDetailsList.get(0);

            if (request.getContainersList() != null && !shipmentSettingsDetails.getMergeContainers()) {
                List<ContainerRequest> containerRequest = request.getContainersList();
                List<Containers> containers = containerDao.updateEntityFromShipmentConsole(convertToEntityList(containerRequest, Containers.class), consolidationDetails.getId());
                consolidationDetails.setContainersList(containers);
            }

            List<PackingRequest> packingRequest = request.getPackingList();
            if (packingRequest != null)
                createPackingsAsync(consolidationDetails, packingRequest);

            List<EventsRequest> eventsRequest = request.getEventsList();
            if (eventsRequest != null)
                createEventsAsync(consolidationDetails, eventsRequest);

            List<FileRepoRequest> fileRepoRequest = request.getFileRepoList();
            if (fileRepoRequest != null)
                createFileRepoAsync(consolidationDetails, fileRepoRequest);

            List<JobRequest> jobRequest = request.getJobsList();
            if (jobRequest != null)
                createJobsAsync(consolidationDetails, jobRequest);

            List<ReferenceNumbersRequest> referenceNumbersRequest = request.getReferenceNumbersList();
            if (referenceNumbersRequest != null)
                createReferenceNumbersAsync(consolidationDetails, referenceNumbersRequest);

            List<RoutingsRequest> routingsRequest = request.getRoutingsList();
            if (routingsRequest != null)
                createRoutingsAsync(consolidationDetails, routingsRequest);

            List<PartiesRequest> consolidationAddressRequest = request.getConsolidationAddresses();
            if (consolidationAddressRequest != null)
                createPartiesAsync(consolidationDetails, consolidationAddressRequest);

            Optional<ConsolidationDetails> consol = consolidationDetailsDao.findById(consolidationDetails.getId());
            try {
                consolidationSync.sync(consol.get());
            } catch (Exception e){
                log.error("Error performing sync on consolidation entity, {}", e);
            }
            afterSave(consol.get(), true);
            // EventMessage eventMessage = EventMessage.builder().messageType(Constants.SERVICE).entity(Constants.CONSOLIDATION).request(consolidationDetails).build();
            // sbUtils.sendMessagesToTopic(isbProperties, azureServiceBusTopic.getTopic(), Arrays.asList(new ServiceBusMessage(jsonHelper.convertToJsonIncludeNulls(eventMessage))));

        } catch (Exception e) {
            log.error(e.getMessage());
            TransactionAspectSupport.currentTransactionStatus().setRollbackOnly();
            throw new RuntimeException(e);
        }
        return ResponseHelper.buildSuccessResponse(jsonHelper.convertValue(consolidationDetails, ConsolidationDetailsResponse.class));
    }

    void getConsolidation(ConsolidationDetails consolidationDetails) throws Exception{
        String responseMsg;
        try {
            generateConsolidationNumber(consolidationDetails);
            consolidationDetails = consolidationDetailsDao.save(consolidationDetails, false);

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(consolidationDetails)
                            .prevData(null)
                            .parent(ConsolidationDetails.class.getSimpleName())
                            .parentId(consolidationDetails.getId())
                            .operation(DBOperationType.CREATE.name()).build()
            );
        }

        catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }

    /**
     * Method that generates consol and bol number
     * @param consolidationDetails
     */
    private void generateConsolidationNumber(ConsolidationDetails consolidationDetails) {
        List<ShipmentSettingsDetails> shipmentSettingsList = shipmentSettingsDao.list();

        if(consolidationDetails.getConsolidationNumber() == null) {
            if(shipmentSettingsList.get(0) != null && shipmentSettingsList.get(0).getCustomisedSequence()) {
                String consoleNumber = getCustomizedConsolidationProcessNumber(consolidationDetails, shipmentSettingsList.get(0), ProductProcessTypes.ReferenceNumber);
                String bol = getCustomizedConsolidationProcessNumber(consolidationDetails, shipmentSettingsList.get(0), ProductProcessTypes.BOLNumber);

                if(bol != null && !bol.isEmpty())
                    consolidationDetails.setBol(bol);
                if(consoleNumber != null && !consoleNumber.isEmpty())
                    consolidationDetails.setConsolidationNumber(consoleNumber);
                if (consolidationDetails.getConsolidationNumber() == null || consolidationDetails.getConsolidationNumber().isEmpty())
                    consolidationDetails.setConsolidationNumber("CONS000" + getConsolidationSerialNumber());
                if (consolidationDetails.getReferenceNumber() == null || consolidationDetails.getReferenceNumber().isEmpty())
                    consolidationDetails.setReferenceNumber(consolidationDetails.getConsolidationNumber());
            }
            else {
                consolidationDetails.setConsolidationNumber("CONS000" + getConsolidationSerialNumber());
                if (consolidationDetails.getReferenceNumber() == null || consolidationDetails.getReferenceNumber().isEmpty())
                    consolidationDetails.setReferenceNumber(consolidationDetails.getConsolidationNumber());
            }
        }
    }

    private String getConsolidationSerialNumber() {
        Long maxId = consolidationDetailsDao.findMaxId();
        if(maxId == null)
            maxId = 0L;
        maxId += 1;
        return maxId.toString();
    }

    private String getCustomizedConsolidationProcessNumber(ConsolidationDetails consolidationDetails, ShipmentSettingsDetails shipmentSettingsDetails, ProductProcessTypes productProcessTypes) {
        productEngine.populateEnabledTenantProducts(shipmentSettingsDetails);
        if (productProcessTypes == ProductProcessTypes.ReferenceNumber) {
            // to check the commmon sequence
            var sequenceNumber = productEngine.GetCommonSequenceNumber(consolidationDetails.getTransportMode(), ProductProcessTypes.Consol_Shipment_TI);
            if (sequenceNumber != null && !sequenceNumber.isEmpty()) {
                return sequenceNumber;
            }
        }
        var identifiedProduct = productEngine.IdentifyProduct(consolidationDetails);
        if (identifiedProduct == null){
            return "";
        }
        var sequenceSettings = GetNextNumberHelper.getProductSequence(identifiedProduct.getId(), productProcessTypes);
        if(sequenceSettings == null){
            return "";
        }
        String prefix = sequenceSettings.getPrefix() == null ? "" : sequenceSettings.getPrefix();
        var user = UserContext.getUser();
        return GetNextNumberHelper.generateCustomSequence(sequenceSettings, prefix, user.getTenantId(), false, null, false);
    }


    private void createPartiesAsync(ConsolidationDetails consolidationDetails, List<PartiesRequest> partiesRequest) {
        partiesRequest.forEach(parties -> {
            createParties(consolidationDetails, parties);
        });
    }

    private void createRoutingsAsync(ConsolidationDetails consolidationDetails, List<RoutingsRequest> routingsRequest) {
        routingsRequest.forEach(routing -> {
            createRouting(consolidationDetails, routing);
        });
    }

    private void createReferenceNumbersAsync(ConsolidationDetails consolidationDetails, List<ReferenceNumbersRequest> referenceNumbersRequest) {
        referenceNumbersRequest.forEach(referenceNumber -> {
            createReferenceNumber(consolidationDetails, referenceNumber);
        });
    }

    private void createNotesAsync(ConsolidationDetails consolidationDetails, List<NotesRequest> notesRequest) {
        notesRequest.forEach(notes -> {
            createNote(consolidationDetails, notes);
        });
    }

    private void createJobsAsync(ConsolidationDetails consolidationDetails, List<JobRequest> jobRequest) {
        jobRequest.forEach(jobs -> {
            createJob(consolidationDetails, jobs);
        });
    }

    private void createFileRepoAsync(ConsolidationDetails consolidationDetails, List<FileRepoRequest> fileRepoRequest) {
        fileRepoRequest.forEach(fileRepo -> {
            createFileRepo(consolidationDetails, fileRepo);
        });
    }

    private void createEventsAsync(ConsolidationDetails consolidationDetails, List<EventsRequest> eventsRequest) {
        eventsRequest.forEach(event -> {
            createEvent(consolidationDetails, event);
        });
    }

    private void createPackingsAsync(ConsolidationDetails consolidationDetails, List<PackingRequest> packingRequest) {
        packingRequest.forEach(packing -> {
            createPacking(consolidationDetails, packing);
        });

    }

    @Transactional
    public void createEvent(ConsolidationDetails consolidationDetails, EventsRequest eventsRequest) {
        eventsRequest.setEntityId(consolidationDetails.getId());
        eventsRequest.setEntityType(Constants.CONSOLIDATION);
        eventDao.save(jsonHelper.convertValue(eventsRequest, Events.class));
    }

    @Transactional
    public void createFileRepo(ConsolidationDetails consolidationDetails, FileRepoRequest fileRepoRequest) {
        fileRepoRequest.setEntityId(consolidationDetails.getId());
        fileRepoRequest.setEntityType(Constants.CONSOLIDATION);
        fileRepoDao.save(jsonHelper.convertValue(fileRepoRequest, FileRepo.class));
    }

    @Transactional
    public void createJob(ConsolidationDetails consolidationDetails, JobRequest jobRequest) {
        jobRequest.setConsolidationId(consolidationDetails.getId());
        jobDao.save(jsonHelper.convertValue(jobRequest, Jobs.class));
    }

    @Transactional
    public void createNote(ConsolidationDetails consolidationDetails, NotesRequest notesRequest) {
        notesRequest.setEntityId(consolidationDetails.getId());
        notesRequest.setEntityType(Constants.CONSOLIDATION);
        notesDao.save(jsonHelper.convertValue(notesRequest, Notes.class));
    }

    @Transactional
    public void createParties(ConsolidationDetails consolidationDetails, PartiesRequest partiesRequest) {
        partiesRequest.setEntityId(consolidationDetails.getId());
        partiesRequest.setEntityType(Constants.CONSOLIDATION_ADDRESSES);
        packingDao.save(jsonHelper.convertValue(partiesRequest, Packing.class));
    }

    @Transactional
    public void createReferenceNumber(ConsolidationDetails consolidationDetails, ReferenceNumbersRequest referenceNumbersRequest) {
        referenceNumbersRequest.setConsolidationId(consolidationDetails.getId());
        referenceNumbersDao.save(jsonHelper.convertValue(referenceNumbersRequest, ReferenceNumbers.class));
    }

    @Transactional
    public void createPacking(ConsolidationDetails consolidationDetails, PackingRequest packingRequest) {
        packingRequest.setConsolidationId(consolidationDetails.getId());
        packingDao.save(jsonHelper.convertValue(packingRequest, Packing.class));
    }

    @Transactional
    public void createRouting(ConsolidationDetails consolidationDetails, RoutingsRequest routingsRequest) {
        routingsRequest.setConsolidationId(consolidationDetails.getId());
        routingsDao.save(jsonHelper.convertValue(routingsRequest, Routings.class));
    }

    @Transactional
    public void createCarrier(CarrierDetails carrierDetails) {
        carrierDao.save(carrierDetails);
    }

    @Transactional
    public void createAllocations(Allocations allocations) {
        allocationsDao.save(allocations);
    }

    @Transactional
    public void createAchievedQuantities(AchievedQuantities achievedQuantities) {
        achievedQuantitiesDao.save(achievedQuantities);
    }

    public Optional<ConsolidationDetails> retrieveByIdOrGuid(ConsolidationDetailsRequest request){
        String responseMsg;

        if (request == null) {
            log.error("Request is empty for Consolidation update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

        Optional<ConsolidationDetails> oldEntity = Optional.ofNullable(null);

        if(request.getId()!=null){
            long id = request.getId();
            oldEntity=consolidationDetailsDao.findById(id);
            if (!oldEntity.isPresent()) {
                log.debug("Consolidation Details is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }

        }

        else if(request.getGuid()!=null){
            UUID guid = request.getGuid();
            oldEntity= consolidationDetailsDao.findByGuid(guid);
            if (!oldEntity.isPresent()) {
                log.debug("Consolidation Details is null for GUID {} with Request GUID {}", request.getGuid(), LoggerHelper.getRequestIdFromMDC());
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
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) {
        String responseMsg;
        ConsolidationDetailsRequest request = (ConsolidationDetailsRequest) commonRequestModel.getData();
        if (request == null) {
            log.error("Request is empty for Consolidation update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
//        if (request.getId() == null) {
//            log.error("Request Id is null for Consolidation update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
//        }
        // TODO- implement Validation logic

        Optional<ConsolidationDetails> oldEntity = retrieveByIdOrGuid(request);
        long id = oldEntity.get().getId();

        if (!oldEntity.isPresent()) {
            log.debug("Consolidation Details is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        ConsolidationDetails entity = jsonHelper.convertValue(request, ConsolidationDetails.class);
        entity.setId(oldEntity.get().getId());
        if(entity.getGuid() != null && !oldEntity.get().getGuid().equals(entity.getGuid())) {
            throw new RunnerException("Provided GUID doesn't match with the existing one !");
        }
        if (entity.getContainersList() == null)
            entity.setContainersList(oldEntity.get().getContainersList());
        entity = consolidationDetailsDao.update(entity, false);
        try {
            consolidationSync.sync(entity);
        } catch (Exception e) {
            log.error("Error performing sync on consol entity, {}", e);
        }
        afterSave(entity, false);
        return ResponseHelper.buildSuccessResponse(jsonHelper.convertValue(entity, ConsolidationDetailsResponse.class));

    }

    @Transactional
    public ResponseEntity<?> attachShipments(Long consolidationId, List<Long> shipmentIds) {

        if(consolidationId != null && shipmentIds!= null && shipmentIds.size() > 0) {
            List<Long> attachedShipmentIds = consoleShipmentMappingDao.assignShipments(consolidationId, shipmentIds);
            for(Long shipId : attachedShipmentIds) {
                ShipmentDetails shipmentDetails = shipmentDao.findById(shipId).get();
                if(shipmentDetails.getContainersList() != null) {
                    List<Containers> containersList = shipmentDetails.getContainersList();
                    for(Containers container : containersList) {
                        container.setConsolidationId(consolidationId);
                    }
                    containersList = containerDao.saveAll(containersList);
                    containerService.afterSaveList(containersList, false);
                }
            }
        }
        Optional<ConsolidationDetails> consol = consolidationDetailsDao.findById(consolidationId);
        try {
            consolidationSync.sync(consol.get());
        }
        catch (Exception e) {
            log.error("Error Syncing Consol");
        }
        return new ResponseEntity<>(HttpStatus.OK);
    }

    @Transactional
    public ResponseEntity<?> detachShipments(Long consolidationId, List<Long> shipmentIds) {
        if(consolidationId != null && shipmentIds!= null && shipmentIds.size() > 0) {
            List<Long> removedShipmentIds = consoleShipmentMappingDao.detachShipments(consolidationId, shipmentIds);
            for(Long shipId : removedShipmentIds) {
                ShipmentDetails shipmentDetails = shipmentDao.findById(shipId).get();
                if(shipmentDetails.getContainersList() != null) {
                    List<Containers> containersList = shipmentDetails.getContainersList();
                    for(Containers container : containersList) {
                        shipmentsContainersMappingDao.detachShipments(container.getId(), List.of(shipId));
                    }
                    containersList = containerDao.saveAll(containersList);
                    containerService.afterSaveList(containersList, false);
                }
            }
        }
        Optional<ConsolidationDetails> consol = consolidationDetailsDao.findById(consolidationId);
        try {
            consolidationSync.sync(consol.get());
        }
        catch (Exception e) {
            log.error("Error Syncing Consol");
        }

        return new ResponseEntity<>(HttpStatus.OK);
    }

    @Transactional
    public ResponseEntity<?> detachShipmentsReverseSync(Long consolidationId, List<Long> shipmentIds) {
        if(consolidationId != null && shipmentIds!= null && shipmentIds.size() > 0) {
            List<Long> removedShipmentIds = consoleShipmentMappingDao.detachShipments(consolidationId, shipmentIds);
            for(Long shipId : removedShipmentIds) {
                ShipmentDetails shipmentDetails = shipmentDao.findById(shipId).get();
                if(shipmentDetails.getContainersList() != null) {
                    List<Containers> containersList = shipmentDetails.getContainersList();
                    for(Containers container : containersList) {
                        shipmentsContainersMappingDao.detachShipments(container.getId(), List.of(shipId));
                    }
                    containerDao.saveAll(containersList);
                }
            }
        }

        return new ResponseEntity<>(HttpStatus.OK);
    }

    @Transactional
    public ResponseEntity<?> partialUpdate(CommonRequestModel commonRequestModel) throws Exception {
        ConsolidationPatchRequest consolidationDetailsRequest = (ConsolidationPatchRequest) commonRequestModel.getData();
        // Get old entity to be updated
        ConsolidationDetailsRequest req = new ConsolidationDetailsRequest();
        req.setId(consolidationDetailsRequest.getId());
        req.setGuid(consolidationDetailsRequest.getGuid());
        Optional<ConsolidationDetails> oldEntity = retrieveByIdOrGuid(req);
        try {
            ConsolidationDetails entity = oldEntity.get();
            long id = oldEntity.get().getId();
            consolidationDetailsMapper.update(consolidationDetailsRequest, entity);
            entity = consolidationDetailsDao.update(entity, false);

            List<PackingRequest> packingRequestList = consolidationDetailsRequest.getPackingList();
            List<ContainerRequest> containerRequestList = consolidationDetailsRequest.getContainersList();
            List<EventsRequest> eventsRequestList = consolidationDetailsRequest.getEventsList();
            List<FileRepoRequest> fileRepoRequestList = consolidationDetailsRequest.getFileRepoList();
            List<JobRequest> jobRequestList = consolidationDetailsRequest.getJobsList();
            List<ReferenceNumbersRequest> referenceNumbersRequestList = consolidationDetailsRequest.getReferenceNumbersList();
            List<RoutingsRequest> routingsRequestList = consolidationDetailsRequest.getRoutingsList();
            List<PartiesRequest> consolidationAddressRequest = consolidationDetailsRequest.getConsolidationAddresses();

            if(containerRequestList != null) {
                List<Containers> updatedContainers = containerDao.updateEntityFromShipmentConsole(convertToEntityList(containerRequestList, Containers.class), id);
                entity.setContainersList(updatedContainers);
            }
            if (packingRequestList != null) {
                List<Packing> updatedPackings = packingDao.updateEntityFromConsole(convertToEntityList(packingRequestList, Packing.class), id);
                entity.setPackingList(updatedPackings);
            }
            if (eventsRequestList != null) {
                List<Events> updatedEvents = eventDao.updateEntityFromOtherEntity(convertToEntityList(eventsRequestList, Events.class), id, Constants.CONSOLIDATION);
                entity.setEventsList(updatedEvents);
            }
            if (fileRepoRequestList != null) {
                List<FileRepo> updatedFileRepos = fileRepoDao.updateEntityFromOtherEntity(convertToEntityList(fileRepoRequestList, FileRepo.class), id, Constants.CONSOLIDATION);
                entity.setFileRepoList(updatedFileRepos);
            }
            if (jobRequestList != null) {
                List<Jobs> updatedJobs = jobDao.updateEntityFromConsole(convertToEntityList(jobRequestList, Jobs.class), id);
                entity.setJobsList(updatedJobs);
            }
            if (referenceNumbersRequestList != null) {
                List<ReferenceNumbers> updatedReferenceNumbers = referenceNumbersDao.updateEntityFromConsole(convertToEntityList(referenceNumbersRequestList, ReferenceNumbers.class), id);
                entity.setReferenceNumbersList(updatedReferenceNumbers);
            }
            if (routingsRequestList != null) {
                List<Routings> updatedRoutings = routingsDao.updateEntityFromConsole(convertToEntityList(routingsRequestList, Routings.class), id);
                entity.setRoutingsList(updatedRoutings);
            }
            if (consolidationAddressRequest != null) {
                List<Parties> updatedFileRepos = partiesDao.updateEntityFromOtherEntity(convertToEntityList(consolidationAddressRequest, Parties.class), id, Constants.CONSOLIDATION_ADDRESSES);
                entity.setConsolidationAddresses(updatedFileRepos);
            }
            try {
                consolidationSync.sync(entity);
            } catch (Exception e){
                log.error("Error performing sync on consolidation entity, {}", e);
            }
            afterSave(entity, false);

            ConsolidationDetailsResponse response = jsonHelper.convertValue(entity, ConsolidationDetailsResponse.class);
            return ResponseHelper.buildSuccessResponse(response);

        } catch (Exception e){
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Transactional
    public ResponseEntity<?> completeUpdate(CommonRequestModel commonRequestModel) throws Exception {

        ConsolidationDetailsRequest consolidationDetailsRequest = (ConsolidationDetailsRequest) commonRequestModel.getData();

        List<PackingRequest> packingRequestList = consolidationDetailsRequest.getPackingList();
        List<ContainerRequest> containerRequestList = consolidationDetailsRequest.getContainersList();
        List<EventsRequest> eventsRequestList = consolidationDetailsRequest.getEventsList();
        List<FileRepoRequest> fileRepoRequestList = consolidationDetailsRequest.getFileRepoList();
        List<JobRequest> jobRequestList = consolidationDetailsRequest.getJobsList();
        List<ReferenceNumbersRequest> referenceNumbersRequestList = consolidationDetailsRequest.getReferenceNumbersList();
        List<RoutingsRequest> routingsRequestList = consolidationDetailsRequest.getRoutingsList();
        List<PartiesRequest> consolidationAddressRequest = consolidationDetailsRequest.getConsolidationAddresses();
        // TODO- implement Validation logic

        Optional<ConsolidationDetails> oldEntity = retrieveByIdOrGuid(consolidationDetailsRequest);
        long id = oldEntity.get().getId();
        if (!oldEntity.isPresent()) {
            log.debug("Consolidation Details is null for Id {}", consolidationDetailsRequest.getId());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        consolidationDetailsRequest.setShipmentsList(null);

        try {

            ConsolidationDetails entity = jsonHelper.convertValue(consolidationDetailsRequest, ConsolidationDetails.class);
            String oldEntityJsonString = jsonHelper.convertToJson(oldEntity.get());
            entity = consolidationDetailsDao.update(entity, false);
            try {
                // audit logs
                auditLogService.addAuditLog(
                        AuditLogMetaData.builder()
                                .newData(entity)
                                .prevData(jsonHelper.readFromJson(oldEntityJsonString, ConsolidationDetails.class))
                                .parent(ConsolidationDetails.class.getSimpleName())
                                .parentId(entity.getId())
                                .operation(DBOperationType.UPDATE.name()).build()
                );
            }
            catch (Exception e) {
                log.error("Error writing audit service log", e);
            }

            List<ShipmentSettingsDetails> shipmentSettingsDetailsList = shipmentSettingsDao.getSettingsByTenantIds(List.of(TenantContext.getCurrentTenant()));
            ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
            if(shipmentSettingsDetailsList.size() > 0)
                shipmentSettingsDetails = shipmentSettingsDetailsList.get(0);
            if(containerRequestList != null && !shipmentSettingsDetails.getMergeContainers()) {
                List<Containers> updatedContainers = containerDao.updateEntityFromShipmentConsole(convertToEntityList(containerRequestList, Containers.class), id);
                entity.setContainersList(updatedContainers);
            }
            if (packingRequestList != null) {
                List<Packing> updatedPackings = packingDao.updateEntityFromConsole(convertToEntityList(packingRequestList, Packing.class), id);
                entity.setPackingList(updatedPackings);
            }
            if (eventsRequestList != null) {
                List<Events> updatedEvents = eventDao.updateEntityFromOtherEntity(convertToEntityList(eventsRequestList, Events.class), id, Constants.CONSOLIDATION);
                entity.setEventsList(updatedEvents);
            }
            if (fileRepoRequestList != null) {
                List<FileRepo> updatedFileRepos = fileRepoDao.updateEntityFromOtherEntity(convertToEntityList(fileRepoRequestList, FileRepo.class), id, Constants.CONSOLIDATION);
                entity.setFileRepoList(updatedFileRepos);
            }
            if (jobRequestList != null) {
                List<Jobs> updatedJobs = jobDao.updateEntityFromConsole(convertToEntityList(jobRequestList, Jobs.class), id);
                entity.setJobsList(updatedJobs);
            }
            if (referenceNumbersRequestList != null) {
                List<ReferenceNumbers> updatedReferenceNumbers = referenceNumbersDao.updateEntityFromConsole(convertToEntityList(referenceNumbersRequestList, ReferenceNumbers.class), id);
                entity.setReferenceNumbersList(updatedReferenceNumbers);
            }
            if (routingsRequestList != null) {
                List<Routings> updatedRoutings = routingsDao.updateEntityFromConsole(convertToEntityList(routingsRequestList, Routings.class), id);
                entity.setRoutingsList(updatedRoutings);
            }
            if (consolidationAddressRequest != null) {
                List<Parties> updatedFileRepos = partiesDao.updateEntityFromOtherEntity(convertToEntityList(consolidationAddressRequest, Parties.class), id, Constants.CONSOLIDATION_ADDRESSES);
                entity.setConsolidationAddresses(updatedFileRepos);
            }
            try {
                consolidationSync.sync(entity);
            } catch (Exception e){
                log.error("Error performing sync on consolidation entity, {}", e);
            }
            afterSave(entity, false);
            ConsolidationDetailsResponse response = jsonHelper.convertValue(entity, ConsolidationDetailsResponse.class);

            return ResponseHelper.buildSuccessResponse(response);
        } catch (ExecutionException e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            TransactionAspectSupport.currentTransactionStatus().setRollbackOnly();
            throw new RuntimeException(e);
        }
    }

    private ConsolidationDetails calculateConsolUtilization(ConsolidationDetails consolidationDetails) throws Exception {
        String responseMsg;
        try {
            if (consolidationDetails.getAchievedQuantities().getConsolidatedWeightUnit() != null && consolidationDetails.getAllocations().getWeightUnit() != null) {
                BigDecimal consolidatedWeight = new BigDecimal(convertUnit(Constants.MASS, consolidationDetails.getAchievedQuantities().getConsolidatedWeight(), consolidationDetails.getAchievedQuantities().getConsolidatedWeightUnit(), Constants.WEIGHT_UNIT_KG).toString());
                BigDecimal weight = new BigDecimal(convertUnit(Constants.MASS, consolidationDetails.getAllocations().getWeight(), consolidationDetails.getAllocations().getWeightUnit(), Constants.WEIGHT_UNIT_KG).toString());
                consolidationDetails.getAchievedQuantities().setWeightUtilization(((consolidatedWeight.divide(weight)).multiply(new BigDecimal(100))).toString());
            }
            if (consolidationDetails.getAchievedQuantities().getConsolidatedVolumeUnit() != null && consolidationDetails.getAllocations().getVolumeUnit() != null) {
                BigDecimal consolidatedVolume = new BigDecimal(convertUnit(Constants.VOLUME, consolidationDetails.getAchievedQuantities().getConsolidatedVolume(), consolidationDetails.getAchievedQuantities().getConsolidatedVolumeUnit(), Constants.VOLUME_UNIT_M3).toString());
                BigDecimal volume = new BigDecimal(convertUnit(Constants.VOLUME, consolidationDetails.getAllocations().getVolume(), consolidationDetails.getAllocations().getVolumeUnit(), Constants.VOLUME_UNIT_M3).toString());
                consolidationDetails.getAchievedQuantities().setVolumeUtilization(((consolidatedVolume.divide(volume, 4, RoundingMode.CEILING)).multiply(new BigDecimal(100))).toString());
            }
            return consolidationDetails;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }

    @Override
    public ResponseEntity<?> calculateUtilization(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ConsolidationDetailsRequest consolidationDetailsRequest = (ConsolidationDetailsRequest) commonRequestModel.getData();
            ConsolidationDetails consolidationDetails = convertToClass(consolidationDetailsRequest, ConsolidationDetails.class);
            consolidationDetails = calculateConsolUtilization(consolidationDetails);
            return ResponseHelper.buildSuccessResponse(convertToClass(consolidationDetails, ConsolidationDetailsResponse.class));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<?> calculateAchieved_AllocatedForSameUnit(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ConsolidationDetailsRequest consolidationDetailsRequest = (ConsolidationDetailsRequest) commonRequestModel.getData();
            ConsolidationDetails consolidationDetails = convertToClass(consolidationDetailsRequest, ConsolidationDetails.class);
            if (consolidationDetails.getAchievedQuantities().getConsolidatedWeightUnit() != consolidationDetails.getAllocations().getWeightUnit()) {
                BigDecimal val = new BigDecimal(convertUnit(Constants.MASS, consolidationDetails.getAchievedQuantities().getConsolidatedWeight(), consolidationDetails.getAchievedQuantities().getConsolidatedWeightUnit(), consolidationDetails.getAllocations().getWeightUnit()).toString());
                consolidationDetails.getAchievedQuantities().setConsolidatedWeight(val);
                consolidationDetails.getAchievedQuantities().setConsolidatedWeightUnit(consolidationDetails.getAllocations().getWeightUnit());
            }
            if (consolidationDetails.getAchievedQuantities().getConsolidatedVolumeUnit() != consolidationDetails.getAllocations().getVolumeUnit()) {
                BigDecimal val = new BigDecimal(convertUnit(Constants.VOLUME, consolidationDetails.getAchievedQuantities().getConsolidatedVolume(), consolidationDetails.getAchievedQuantities().getConsolidatedVolumeUnit(), consolidationDetails.getAllocations().getVolumeUnit()).toString());
                consolidationDetails.getAchievedQuantities().setConsolidatedVolume(val);
                consolidationDetails.getAchievedQuantities().setConsolidatedVolumeUnit(consolidationDetails.getAllocations().getVolumeUnit());
            }
            return ResponseHelper.buildSuccessResponse(convertToClass(consolidationDetails, ConsolidationDetailsResponse.class));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<?> calculateChargeable(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ConsolidationDetailsRequest consolidationDetailsRequest = (ConsolidationDetailsRequest) commonRequestModel.getData();
            ConsolidationDetails consolidationDetails = convertToClass(consolidationDetailsRequest, ConsolidationDetails.class);
            String transportMode = consolidationDetails.getTransportMode();
            BigDecimal weight = consolidationDetails.getAllocations().getWeight();
            String weightUnit = consolidationDetails.getAllocations().getWeightUnit();
            BigDecimal volume = consolidationDetails.getAllocations().getVolume();
            String volumeUnit = consolidationDetails.getAllocations().getVolumeUnit();
            if (weightUnit != null && volumeUnit != null) {
                VolumeWeightChargeable vwOb = calculateVolumeWeight(consolidationDetails, transportMode, weightUnit, volumeUnit, weight, volume);
                consolidationDetails.getAllocations().setChargable(vwOb.getChargeable());
                if (transportMode == Constants.TRANSPORT_MODE_AIR) {
                    BigDecimal charge = consolidationDetails.getAllocations().getChargable();
                    BigDecimal half = new BigDecimal("0.50");
                    BigDecimal floor = charge.setScale(0, BigDecimal.ROUND_FLOOR);
                    if (charge.subtract(half).compareTo(floor) <= 0 && charge.compareTo(floor) != 0) {
                        charge = floor.add(half);
                    } else {
                        charge = charge.setScale(0, BigDecimal.ROUND_CEILING);
                    }
                    consolidationDetails.getAllocations().setChargable(charge);
                }
                if (transportMode.equals(Constants.TRANSPORT_MODE_SEA) && consolidationDetails.getShipmentType().equals(Constants.SHIPMENT_TYPE_LCL)) {
                    volume = new BigDecimal(convertUnit(Constants.VOLUME, volume, volumeUnit, Constants.VOLUME_UNIT_M3).toString());
                    weight = new BigDecimal(convertUnit(Constants.MASS, weight, weightUnit, Constants.WEIGHT_UNIT_KG).toString());
                    consolidationDetails.getAllocations().setChargable(weight.divide(new BigDecimal("1000")).max(volume));
                    vwOb = calculateVolumeWeight(consolidationDetails, transportMode, Constants.WEIGHT_UNIT_KG, Constants.VOLUME_UNIT_M3, weight, volume);
                }
                consolidationDetails.getAllocations().setChargeableUnit(vwOb.getChargeableUnit());
            }
            return ResponseHelper.buildSuccessResponse(convertToClass(consolidationDetails, ConsolidationDetailsResponse.class));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private VolumeWeightChargeable calculateVolumeWeight(ConsolidationDetails consolidationDetails, String transportMode, String weightUnit, String volumeUnit, BigDecimal weight, BigDecimal volume) throws Exception {
        String responseMsg;
        try {
            VolumeWeightChargeable vwOb = new VolumeWeightChargeable();
            if (!weightUnit.isEmpty() && !volumeUnit.isEmpty() && !transportMode.isEmpty()) {
                switch (transportMode) {
                    case Constants.TRANSPORT_MODE_SEA:
                    case Constants.TRANSPORT_MODE_RAI:
                    case Constants.TRANSPORT_MODE_FSA:
                        BigDecimal volInM3 = new BigDecimal(convertUnit(Constants.VOLUME, volume, volumeUnit, Constants.VOLUME_UNIT_M3).toString());
                        vwOb.setChargeable(volInM3.multiply(BigDecimal.valueOf(10)).setScale(0, BigDecimal.ROUND_CEILING).divide(BigDecimal.valueOf(10)));
                        vwOb.setChargeableUnit(Constants.VOLUME_UNIT_M3);

                        BigDecimal wtInTn = new BigDecimal(convertUnit(Constants.MASS, weight, weightUnit, Constants.WEIGHT_UNIT_KG).toString());
                        wtInTn = wtInTn.divide(BigDecimal.valueOf(1000));
                        BigDecimal wv = new BigDecimal(convertUnit(Constants.VOLUME, wtInTn, Constants.VOLUME_UNIT_M3, volumeUnit).toString());
                        vwOb.setVolumeWeight(wv.multiply(BigDecimal.TEN).setScale(0, RoundingMode.CEILING).divide(BigDecimal.TEN));
                        vwOb.setVolumeWeightUnit(volumeUnit);
                        break;
                    case Constants.TRANSPORT_MODE_AIR:
                    case Constants.TRANSPORT_MODE_FAS:
                    case Constants.TRANSPORT_MODE_ROA:
                        BigDecimal wtInKG = new BigDecimal(convertUnit(Constants.MASS, weight, weightUnit, Constants.WEIGHT_UNIT_KG).toString());
                        BigDecimal vlInM3 = new BigDecimal(convertUnit(Constants.VOLUME, volume, volumeUnit, Constants.VOLUME_UNIT_M3).toString());
                        BigDecimal factor = new BigDecimal(166.667);
                        if (transportMode == Constants.TRANSPORT_MODE_ROA) {
                            factor = BigDecimal.valueOf(333.0);
                        }
                        BigDecimal wvInKG = vlInM3.multiply(factor);
                        if (wtInKG.compareTo(wvInKG) < 0) {
                            wtInKG = wvInKG;
                        }
                        vwOb.setChargeable(wtInKG.multiply(BigDecimal.valueOf(100)).setScale(0, BigDecimal.ROUND_CEILING).divide(BigDecimal.valueOf(100)));
                        vwOb.setChargeableUnit(Constants.WEIGHT_UNIT_KG);
                        BigDecimal WV = new BigDecimal(convertUnit(Constants.MASS, wvInKG, Constants.WEIGHT_UNIT_KG, weightUnit).toString());
                        vwOb.setVolumeWeight(WV);
                        vwOb.setVolumeWeightUnit(weightUnit);
                        break;
                }
            }
            return vwOb;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }

    @Override
    public ResponseEntity<?> calculateAchievedValues(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ConsolidationDetailsRequest consolidationDetailsRequest = (ConsolidationDetailsRequest) commonRequestModel.getData();
            ConsolidationDetails consolidationDetails = convertToClass(consolidationDetailsRequest, ConsolidationDetails.class);
            if (consolidationDetails.getOverride() != null && consolidationDetails.getOverride()) {
                return ResponseHelper.buildSuccessResponse(convertToClass(consolidationDetails, ConsolidationDetailsResponse.class));
            }
            String weightChargeableUnit = Constants.WEIGHT_UNIT_KG; // TODO- Actually fetch from tenant Settings
            String volumeChargeableUnit = Constants.VOLUME_UNIT_M3; // TODO- Actually fetch from tenant Settings
            BigDecimal sumWeight = new BigDecimal(0);
            BigDecimal sumVolume = new BigDecimal(0);
            if (consolidationDetails.getShipmentsList() != null && !consolidationDetails.getShipmentsList().isEmpty()) {
                for (ShipmentDetails shipmentDetails : consolidationDetails.getShipmentsList()) {
                    sumWeight = sumWeight.add(new BigDecimal(convertUnit(Constants.MASS, shipmentDetails.getWeight(), shipmentDetails.getWeightUnit(), weightChargeableUnit).toString()));
                    sumVolume = sumVolume.add(new BigDecimal(convertUnit(Constants.VOLUME, shipmentDetails.getVolume(), shipmentDetails.getVolumeUnit(), volumeChargeableUnit).toString()));
                }
                consolidationDetails.getAllocations().setShipmentsCount(consolidationDetails.getShipmentsList().size());
            }
            consolidationDetails.getAchievedQuantities().setConsolidatedWeight(sumWeight);
            consolidationDetails.getAchievedQuantities().setConsolidatedWeightUnit(weightChargeableUnit);
            consolidationDetails.getAchievedQuantities().setConsolidatedVolume(sumVolume);
            consolidationDetails.getAchievedQuantities().setConsolidatedVolumeUnit(volumeChargeableUnit);

            consolidationDetails = calculateConsolUtilization(consolidationDetails);

            String transportMode = consolidationDetails.getTransportMode();
            BigDecimal weight = consolidationDetails.getAllocations().getWeight();
            String weightUnit = consolidationDetails.getAllocations().getWeightUnit();
            BigDecimal volume = consolidationDetails.getAllocations().getVolume();
            String volumeUnit = consolidationDetails.getAllocations().getVolumeUnit();
            VolumeWeightChargeable vwOb = calculateVolumeWeight(consolidationDetails, transportMode, weightUnit, volumeUnit, weight, volume);

            consolidationDetails.getAchievedQuantities().setConsolidationChargeQuantity(vwOb.getChargeable());
            consolidationDetails.getAchievedQuantities().setConsolidationChargeQuantityUnit(vwOb.getChargeableUnit());
            if (transportMode.equals(Constants.TRANSPORT_MODE_SEA) && consolidationDetails.getShipmentType().equals(Constants.SHIPMENT_TYPE_LCL)) {
                BigDecimal winKg = new BigDecimal(convertUnit(Constants.MASS, consolidationDetails.getAllocations().getWeight(), consolidationDetails.getAllocations().getWeightUnit(), Constants.WEIGHT_UNIT_KG).toString());
                BigDecimal vinM3 = new BigDecimal(convertUnit(Constants.VOLUME, consolidationDetails.getAllocations().getVolume(), consolidationDetails.getAllocations().getVolumeUnit(), Constants.VOLUME_UNIT_M3).toString());
                consolidationDetails.getAchievedQuantities().setConsolidationChargeQuantity(winKg.divide(BigDecimal.valueOf(1000)).max(vinM3));
                consolidationDetails.getAchievedQuantities().setConsolidationChargeQuantityUnit(Constants.VOLUME_UNIT_M3);
            }
            consolidationDetails.getAchievedQuantities().setWeightVolume(vwOb.getVolumeWeight());
            consolidationDetails.getAchievedQuantities().setWeightVolumeUnit(vwOb.getVolumeWeightUnit());
            consolidationDetails.getAllocations().setChargeableUnit(vwOb.getChargeableUnit());
            return ResponseHelper.buildSuccessResponse(convertToClass(consolidationDetails, ConsolidationDetailsResponse.class));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public ResponseEntity<?> completeRetrieveById(CommonRequestModel commonRequestModel) throws ExecutionException, InterruptedException {
        try {
            // create common list request for consolidation id
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for  Consolidation retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.error("Request Id is null for Consolidation complete retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();

            CompletableFuture<ResponseEntity<?>> consolidationsFuture = retrieveByIdAsync(commonRequestModel);
            RunnerResponse<ConsolidationDetailsResponse> res = (RunnerResponse<ConsolidationDetailsResponse>) consolidationsFuture.get().getBody();
            if(request.getIncludeColumns()==null||request.getIncludeColumns().size()==0)
            return ResponseHelper.buildSuccessResponse(res.getData());
            else{
                return ResponseHelper.buildSuccessResponse(PartialFetchUtils.fetchPartialListData(res.getData(), request.getIncludeColumns()));
            }
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public ResponseEntity<?> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            // TODO- implement actual logic with filters
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Consolidation list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            Pair<Specification<ConsolidationDetails>, Pageable> tuple = fetchData(request, ConsolidationDetails.class, tableNames);
            Page<ConsolidationDetails> consolidationDetailsPage = consolidationDetailsDao.findAll(tuple.getLeft(), tuple.getRight());
            List<IRunnerResponse> consoleResponse = convertEntityListToDtoList(consolidationDetailsPage.getContent());
            log.info("Consolidation list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildListSuccessResponse(
                    consoleResponse,
                    consolidationDetailsPage.getTotalPages(),
                    consolidationDetailsPage.getTotalElements());
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
                log.error("Request is empty for Consolidation async list for Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            Pair<Specification<ConsolidationDetails>, Pageable> tuple = fetchData(request, ConsolidationDetails.class, tableNames);
            Page<ConsolidationDetails> consolidationDetailsPage = consolidationDetailsDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Consolidation async list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return CompletableFuture.completedFuture(ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(consolidationDetailsPage.getContent()),
                    consolidationDetailsPage.getTotalPages(),
                    consolidationDetailsPage.getTotalElements()));
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
                log.debug("Request is empty for Consolidation delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.debug("Request Id is null for Consolidation delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findById(id);
            if (!consolidationDetails.isPresent()) {
                log.debug("Consolidation Details is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            String oldEntityJsonString = jsonHelper.convertToJson(consolidationDetails.get());
            consolidationDetailsDao.delete(consolidationDetails.get());
            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(null)
                            .prevData(jsonHelper.readFromJson(oldEntityJsonString, ConsolidationDetails.class))
                            .parent(ConsolidationDetails.class.getSimpleName())
                            .parentId(consolidationDetails.get().getId())
                            .operation(DBOperationType.DELETE.name()).build()
            );

            log.info("Deleted Consolidation details for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
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
                log.error("Request is empty for Consolidation retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.error("Request Id is null for Consolidation retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findById(id);
            if (!consolidationDetails.isPresent()) {
                log.debug("Consolidation Details is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("Consolidation details fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            List<ShipmentSettingsDetails> shipmentSettingsDetailsList = shipmentSettingsDao.getSettingsByTenantIds(List.of(TenantContext.getCurrentTenant()));
            ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
            if(shipmentSettingsDetailsList.size() > 0) {
                shipmentSettingsDetails = shipmentSettingsDetailsList.get(0);
            }
            if(shipmentSettingsDetails.getMergeContainers() && consolidationDetails.get().getContainersList() != null && consolidationDetails.get().getContainersList().size() > 0) {
                consolidationDetails.get().setContainersList(mergeContainers(consolidationDetails.get().getContainersList(), shipmentSettingsDetails));
            }
            ConsolidationDetailsResponse response = jsonHelper.convertValue(consolidationDetails.get(), ConsolidationDetailsResponse.class);
            if(response != null && response.getGuid() != null) {
                List<UUID> guidsList = new ArrayList<>();
                guidsList.add(response.getGuid());
                ConsoleBookingListRequest consoleBookingListRequest = new ConsoleBookingListRequest();
                consoleBookingListRequest.setGuidsList(guidsList);
                ConsoleBookingListResponse consoleBookingListResponse = v1Service.fetchConsolidationBookingData(consoleBookingListRequest);
                if(consoleBookingListResponse.getData() != null && !consoleBookingListResponse.getData().isEmpty()) {
                    if(consoleBookingListResponse.getData().get(response.getGuid()) != null) {
                        response.setBookingStatus(consoleBookingListResponse.getData().get(response.getGuid()).getStatus());
                        response.setBookingId(consoleBookingListResponse.getData().get(response.getGuid()).getIntraBookingId());
                        response.setBookingNumber(consoleBookingListResponse.getData().get(response.getGuid()).getBookingNumber());
                    }
                }
            }
            createConsolidationPayload(consolidationDetails.get(), response);
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
                log.error("Request is empty for Consolidation async retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.error("Request Id is null for Consolidation async retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findById(id);
            if (!consolidationDetails.isPresent()) {
                log.debug("Consolidation Details is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("Consolidation details async fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            List<ShipmentSettingsDetails> shipmentSettingsDetailsList = shipmentSettingsDao.getSettingsByTenantIds(List.of(TenantContext.getCurrentTenant()));
            ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
            if(shipmentSettingsDetailsList.size() > 0) {
                shipmentSettingsDetails = shipmentSettingsDetailsList.get(0);
            }
            if(shipmentSettingsDetails.getMergeContainers() && consolidationDetails.get().getContainersList() != null && consolidationDetails.get().getContainersList().size() > 0) {
                consolidationDetails.get().setContainersList(mergeContainers(consolidationDetails.get().getContainersList(), shipmentSettingsDetails));
            }
            ConsolidationDetailsResponse response = jsonHelper.convertValue(consolidationDetails.get(), ConsolidationDetailsResponse.class);
            if(response != null && response.getGuid() != null) {
                List<UUID> guidsList = new ArrayList<>();
                guidsList.add(response.getGuid());
                ConsoleBookingListRequest consoleBookingListRequest = new ConsoleBookingListRequest();
                consoleBookingListRequest.setGuidsList(guidsList);
                ConsoleBookingListResponse consoleBookingListResponse = v1Service.fetchConsolidationBookingData(consoleBookingListRequest);
                if(consoleBookingListResponse.getData() != null && !consoleBookingListResponse.getData().isEmpty()) {
                    if(consoleBookingListResponse.getData().get(response.getGuid()) != null) {
                        response.setBookingStatus(consoleBookingListResponse.getData().get(response.getGuid()).getStatus());
                        response.setBookingId(consoleBookingListResponse.getData().get(response.getGuid()).getIntraBookingId());
                        response.setBookingNumber(consoleBookingListResponse.getData().get(response.getGuid()).getBookingNumber());
                    }
                }
            }
            createConsolidationPayload(consolidationDetails.get(), response);
            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(response));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return CompletableFuture.completedFuture(ResponseHelper.buildFailedResponse(responseMsg));
        }
    }

    private void createConsolidationPayload (ConsolidationDetails consolidationDetails, ConsolidationDetailsResponse consolidationDetailsResponse) {
        try {
            this.addAllMasterDataInSingleCall(consolidationDetails, consolidationDetailsResponse);
            this.addAllUnlocationDataInSingleCall(consolidationDetails, consolidationDetailsResponse);
            this.addAllCarrierDataInSingleCall(consolidationDetails, consolidationDetailsResponse);
            this.addAllCurrencyDataInSingleCall(consolidationDetails, consolidationDetailsResponse);
            this.addAllCommodityTypesInSingleCall(consolidationDetails,consolidationDetailsResponse);
            this.addAllTenantDataInSingleCall(consolidationDetails, consolidationDetailsResponse);
            this.addAllWarehouseDataInSingleCall(consolidationDetails, consolidationDetailsResponse);
        }  catch (Exception ex) {
            log.error("Request: {} || Error occured for event: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), IntegrationType.MASTER_DATA_FETCH_FOR_CONSOLIDATION_RETRIEVE, ex.getLocalizedMessage());
        }
    }

    private void addAllMasterDataInSingleCall (ConsolidationDetails consolidationDetails, ConsolidationDetailsResponse consolidationDetailsResponse) {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        List<MasterListRequest> listRequests = new ArrayList<>(masterDataUtils.createInBulkMasterListRequest(consolidationDetailsResponse, ConsolidationDetails.class, fieldNameKeyMap, ConsolidationDetails.class.getSimpleName() ));
        if (!Objects.isNull(consolidationDetailsResponse.getCarrierDetails()))
            listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(consolidationDetailsResponse.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName() ));

        Map<String, EntityTransferMasterLists> keyMasterDataMap = masterDataUtils.fetchInBulkMasterList(listRequests);
        masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.MASTER_LIST);

        consolidationDetailsResponse.setMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(ConsolidationDetails.class.getSimpleName()), CacheConstants.MASTER_LIST));
        if (!Objects.isNull(consolidationDetailsResponse.getCarrierDetails()))
            consolidationDetailsResponse.getCarrierDetails().setMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName()), CacheConstants.MASTER_LIST) );
    }

    private void addAllUnlocationDataInSingleCall (ConsolidationDetails consolidationDetails, ConsolidationDetailsResponse consolidationDetailsResponse) {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        List<String> locationCodes = new ArrayList<>(masterDataUtils.createInBulkUnLocationsRequest(consolidationDetailsResponse, ConsolidationDetails.class, fieldNameKeyMap, ConsolidationDetails.class.getSimpleName() ));
        if (!Objects.isNull(consolidationDetailsResponse.getCarrierDetails()))
            locationCodes.addAll((masterDataUtils.createInBulkUnLocationsRequest(consolidationDetailsResponse.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName() )));
        // TODO: This needs to be change to fetch based on LocationServiceGuid once UI is ready
        Map<String, EntityTransferUnLocations> keyMasterDataMap = masterDataUtils.fetchInBulkUnlocations(locationCodes, EntityTransferConstants.UNLOCATION_CODE);
        masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.UNLOCATIONS);

        consolidationDetailsResponse.setUnlocationData(masterDataUtils.setMasterData(fieldNameKeyMap.get(ConsolidationDetails.class.getSimpleName()), CacheConstants.UNLOCATIONS) );
        if (!Objects.isNull(consolidationDetailsResponse.getCarrierDetails()))
            consolidationDetailsResponse.getCarrierDetails().setUnlocationData(masterDataUtils.setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName()), CacheConstants.UNLOCATIONS));
    }

    private void addAllCarrierDataInSingleCall (ConsolidationDetails consolidationDetails, ConsolidationDetailsResponse consolidationDetailsResponse) {
        if (!Objects.isNull(consolidationDetailsResponse.getCarrierDetails())) {
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            List<String> carrierList = new ArrayList<>(masterDataUtils.createInBulkCarriersRequest(consolidationDetailsResponse.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName()));
            Map v1Data = masterDataUtils.fetchInBulkCarriers(carrierList);
            masterDataUtils.pushToCache(v1Data, CacheConstants.CARRIER);
            consolidationDetailsResponse.getCarrierDetails().setCarrierMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName()), CacheConstants.CARRIER));
        }
    }

    private void addAllCurrencyDataInSingleCall (ConsolidationDetails consolidationDetails, ConsolidationDetailsResponse consolidationDetailsResponse) {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        List<String> currencyList = new ArrayList<>(masterDataUtils.createInBulkCurrencyRequest(consolidationDetailsResponse, ConsolidationDetails.class, fieldNameKeyMap, ConsolidationDetails.class.getSimpleName()));
        Map v1Data = masterDataUtils.fetchInCurrencyList(currencyList);
        masterDataUtils.pushToCache(v1Data, CacheConstants.CURRENCIES);
        consolidationDetailsResponse.setCurrenciesMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(ConsolidationDetails.class.getSimpleName()), CacheConstants.CURRENCIES));
    }

    private void addAllCommodityTypesInSingleCall(ConsolidationDetails consolidationDetails, ConsolidationDetailsResponse consolidationDetailsResponse) {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        Set<String> containerTypes = new HashSet<>();
        if (!Objects.isNull(consolidationDetailsResponse.getContainersList()))
            consolidationDetailsResponse.getContainersList().forEach(r -> containerTypes.addAll(masterDataUtils.createInBulkCommodityTypeRequest(r, Containers.class, fieldNameKeyMap, Containers.class.getSimpleName() + r.getId() )));

        Map<String, EntityTransferCommodityType> v1Data = masterDataUtils.fetchInBulkCommodityTypes(containerTypes.stream().toList());
        masterDataUtils.pushToCache(v1Data, CacheConstants.COMMODITY);

        if (!Objects.isNull(consolidationDetailsResponse.getContainersList()))
            consolidationDetailsResponse.getContainersList().forEach(r -> r.setCommodityTypeData(masterDataUtils.setMasterData(fieldNameKeyMap.get(Containers.class.getSimpleName() + r.getId()), CacheConstants.COMMODITY)));
    }

    private void addAllTenantDataInSingleCall (ConsolidationDetails consolidationDetails, ConsolidationDetailsResponse consolidationDetailsResponse) {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        List<String> tenantIdList = new ArrayList<>(masterDataUtils.createInBulkTenantsRequest(consolidationDetailsResponse, ConsolidationDetails.class, fieldNameKeyMap, ConsolidationDetails.class.getSimpleName()));
        Map v1Data = masterDataUtils.fetchInTenantsList(tenantIdList);
        masterDataUtils.pushToCache(v1Data, CacheConstants.TENANTS);
        consolidationDetailsResponse.setTenantIdsData(masterDataUtils.setMasterData(fieldNameKeyMap.get(ConsolidationDetails.class.getSimpleName()), CacheConstants.TENANTS));
    }

    private void addAllWarehouseDataInSingleCall (ConsolidationDetails consolidationDetails, ConsolidationDetailsResponse consolidationDetailsResponse) {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        Set<String> wareHouseTypes = new HashSet<>(masterDataUtils.createInBulkWareHouseRequest(consolidationDetailsResponse, ConsolidationDetails.class, fieldNameKeyMap, ConsolidationDetails.class.getSimpleName()));

        Map v1Data = masterDataUtils.fetchInWareHousesList(wareHouseTypes.stream().toList());
        masterDataUtils.pushToCache(v1Data, CacheConstants.WAREHOUSES);

        consolidationDetailsResponse.setTextData(masterDataUtils.setMasterData(fieldNameKeyMap.get(ConsolidationDetails.class.getSimpleName()), CacheConstants.WAREHOUSES));
    }

    private List<Containers> mergeContainers(List<Containers> containersList, ShipmentSettingsDetails shipmentSettingsDetails) throws Exception{
        try {
            List<Containers> response = new ArrayList<>();
            HashMap<String, List<Containers>> map = new HashMap<>();
            for(Containers container: containersList) {
                if(IsStringNullOrEmpty(container.getContainerCode()) || IsStringNullOrEmpty(container.getContainerNumber()) || IsStringNullOrEmpty(container.getHblDeliveryMode())) {
                    response.add(container);
                    continue;
                }
                String key = container.getContainerCode() + container.getContainerNumber() + container.getHblDeliveryMode();
                if(map.containsKey(key)) {
                    List<Containers> temp = map.get(key);
                    temp.add(container);
                    map.put(key, temp);
                }
                else {
                    List<Containers> containersList1 = new ArrayList<>();
                    containersList1.add(container);
                    map.put(key, containersList1);
                }
            }
            if(map.size() > 0) {
                for (Map.Entry<String, List<Containers>> entry : map.entrySet()) {
                    List<Containers> value = entry.getValue();
                    if(value.size() == 1) {
                        response.add(value.get(0));
                    }
                    else if(entry.getValue().size() > 0){
                        Containers finalContainer = value.get(0);
                        finalContainer.setId(null);
                        finalContainer.setGuid(null);
                        for (int i=1; i < value.size(); i++) {
                            finalContainer = convertIntoMergedConts(finalContainer, value.get(i), shipmentSettingsDetails);
                        }
                        response.add(finalContainer);
                    }
                }
            }
            return response;
        } catch (Exception e) {
            log.error("Error while merging consol containers");
            throw new Exception(e);
        }
    }

    private Containers convertIntoMergedConts(Containers finalContainer, Containers container, ShipmentSettingsDetails shipmentSettingsDetails) throws Exception{
        try {
            finalContainer.setDescriptionOfGoods(mergeTextField(finalContainer.getDescriptionOfGoods(), container.getDescriptionOfGoods()));
//        finalContainer.setHa(mergeTextField(finalContainer.getDescriptionOfGoods(), container.getDescriptionOfGoods())); TODO- missing handling info in containers
            if(finalContainer.getNoOfPackages() != null && container.getNoOfPackages() != null)
                finalContainer.setNoOfPackages(finalContainer.getNoOfPackages() + container.getNoOfPackages());
            else if(container.getNoOfPackages() != null)
                finalContainer.setNoOfPackages(container.getNoOfPackages());
            if(!IsStringNullOrEmpty(finalContainer.getPacks()) && !IsStringNullOrEmpty(container.getPacks()))
                finalContainer.setPacks(String.valueOf(new BigDecimal(finalContainer.getPacks()).add(new BigDecimal(container.getPacks()))));
            else if(!IsStringNullOrEmpty(container.getPacks())) {
                finalContainer.setPacks(container.getPacks());
            }
            finalContainer.setMarksNums(mergeTextField(finalContainer.getMarksNums(), container.getMarksNums()));
            finalContainer.setCustomsReleaseCode(mergeTextField(finalContainer.getCustomsReleaseCode(), container.getCustomsReleaseCode()));
            finalContainer.setPacrNumber(mergeTextField(finalContainer.getPacrNumber(), container.getPacrNumber()));
            finalContainer.setContainerComments(mergeTextField(finalContainer.getContainerComments(), container.getContainerComments()));
            if(!IsStringNullOrEmpty(finalContainer.getPacksType()) && !IsStringNullOrEmpty(container.getPacksType()) && !container.getPacksType().equals(finalContainer.getPacksType()))
                finalContainer.setPacksType(Constants.MPK);
            // Net Weight field
            if(finalContainer.getNetWeight() != null && container.getNetWeight() != null && finalContainer.getNetWeightUnit() != null && container.getNetWeightUnit() != null) {
                if(finalContainer.getNetWeightUnit().equals(container.getNetWeightUnit()))
                    finalContainer.setNetWeight(finalContainer.getNetWeight().add(container.getNetWeight()));
                else {
                    finalContainer.setNetWeight( new BigDecimal(convertUnit(Constants.MASS, finalContainer.getNetWeight(), finalContainer.getNetWeightUnit(), shipmentSettingsDetails.getWeightChargeableUnit()).doubleValue() +
                                    convertUnit(Constants.MASS, container.getNetWeight(), container.getNetWeightUnit(), shipmentSettingsDetails.getWeightChargeableUnit()).doubleValue() ));
                    finalContainer.setNetWeightUnit(shipmentSettingsDetails.getWeightChargeableUnit());
                }
            }
            else if(container.getNetWeight() != null){
                finalContainer.setNetWeightUnit(container.getNetWeightUnit());
                finalContainer.setNetWeight(container.getNetWeight());
            }
            // Gross Weight field
            if(finalContainer.getGrossWeight() != null && container.getGrossWeight() != null && finalContainer.getGrossWeightUnit() != null && container.getGrossWeightUnit() != null) {
                if(finalContainer.getGrossWeightUnit().equals(container.getGrossWeightUnit()))
                    finalContainer.setGrossWeight(finalContainer.getGrossWeight().add(container.getGrossWeight()));
                else {
                    finalContainer.setGrossWeight( new BigDecimal(convertUnit(Constants.MASS, finalContainer.getGrossWeight(), finalContainer.getGrossWeightUnit(), shipmentSettingsDetails.getWeightChargeableUnit()).doubleValue() +
                            convertUnit(Constants.MASS, container.getGrossWeight(), container.getGrossWeightUnit(), shipmentSettingsDetails.getWeightChargeableUnit()).doubleValue() ));
                    finalContainer.setGrossWeightUnit(shipmentSettingsDetails.getWeightChargeableUnit());
                }
            }
            else if(container.getGrossWeight() != null){
                finalContainer.setGrossWeightUnit(container.getGrossWeightUnit());
                finalContainer.setGrossWeight(container.getGrossWeight());
            }
            // Tare Weight field
            if(finalContainer.getTareWeight() != null && container.getTareWeight() != null && finalContainer.getTareWeightUnit() != null && container.getTareWeightUnit() != null) {
                if(finalContainer.getTareWeightUnit().equals(container.getTareWeightUnit()))
                    finalContainer.setTareWeight(finalContainer.getTareWeight().add(container.getTareWeight()));
                else {
                    finalContainer.setTareWeight( new BigDecimal(convertUnit(Constants.MASS, finalContainer.getTareWeight(), finalContainer.getTareWeightUnit(), shipmentSettingsDetails.getWeightChargeableUnit()).doubleValue() +
                            convertUnit(Constants.MASS, container.getTareWeight(), container.getTareWeightUnit(), shipmentSettingsDetails.getWeightChargeableUnit()).doubleValue() ));
                    finalContainer.setTareWeightUnit(shipmentSettingsDetails.getWeightChargeableUnit());
                }
            }
            else if(container.getTareWeight() != null){
                finalContainer.setTareWeightUnit(container.getTareWeightUnit());
                finalContainer.setTareWeight(container.getTareWeight());
            }
            // Gross Volume field
            if(finalContainer.getGrossVolume() != null && container.getGrossVolume() != null && finalContainer.getGrossVolumeUnit() != null && container.getGrossVolumeUnit() != null) {
                if(finalContainer.getGrossVolumeUnit().equals(container.getGrossVolumeUnit()))
                    finalContainer.setGrossVolume(finalContainer.getGrossVolume().add(container.getGrossVolume()));
                else {
                    finalContainer.setGrossVolume( new BigDecimal(convertUnit(Constants.VOLUME, finalContainer.getGrossVolume(), finalContainer.getGrossVolumeUnit(), shipmentSettingsDetails.getVolumeChargeableUnit()).doubleValue() +
                            convertUnit(Constants.VOLUME, container.getGrossVolume(), container.getGrossVolumeUnit(), shipmentSettingsDetails.getVolumeChargeableUnit()).doubleValue() ));
                    finalContainer.setGrossVolumeUnit(shipmentSettingsDetails.getVolumeChargeableUnit());
                }
            }
            else if(container.getGrossVolume() != null){
                finalContainer.setGrossVolumeUnit(container.getGrossVolumeUnit());
                finalContainer.setGrossVolume(container.getGrossVolume());
            }
            // Measurement field
            if(finalContainer.getMeasurement() != null && container.getMeasurement() != null && finalContainer.getMeasurementUnit() != null && container.getMeasurementUnit() != null) {
                if(finalContainer.getMeasurementUnit().equals(container.getMeasurementUnit()))
                    finalContainer.setMeasurement(finalContainer.getMeasurement().add(container.getMeasurement()));
                else {
                    finalContainer.setMeasurement( new BigDecimal(convertUnit(Constants.LENGTH, finalContainer.getMeasurement(), finalContainer.getMeasurementUnit(), shipmentSettingsDetails.getMeasurementChargeableUnit()).doubleValue() +
                            convertUnit(Constants.LENGTH, container.getMeasurement(), container.getMeasurementUnit(), shipmentSettingsDetails.getMeasurementChargeableUnit()).doubleValue() ));
                    finalContainer.setMeasurementUnit(shipmentSettingsDetails.getMeasurementChargeableUnit());
                }
            }
            else if(container.getMeasurement() != null){
                finalContainer.setMeasurementUnit(container.getMeasurementUnit());
                finalContainer.setMeasurement(container.getMeasurement());
            }
            return finalContainer;
        } catch (Exception e) {
            throw new Exception(e);
        }
    }

    private String mergeTextField(String a, String b) {
        if(!IsStringNullOrEmpty(b) && !IsStringNullOrEmpty(b))
            return a + "\n" + b;
        if(!IsStringNullOrEmpty(b))
            return b;
        if(!IsStringNullOrEmpty(a))
            return a;
        return null;
    }

    public ResponseEntity<?> toggleLock(CommonRequestModel commonRequestModel) {
        CommonGetRequest commonGetRequest = (CommonGetRequest) commonRequestModel.getData();
        Long id = commonGetRequest.getId();
        ConsolidationDetails consolidationDetails = consolidationDetailsDao.findById(id).get();
        String lockingUser = consolidationDetails.getLockedBy();
        String currentUser = userContext.getUser().getUsername();

        if (consolidationDetails.getIsLocked()) {
            if (lockingUser != null && lockingUser.equals(currentUser))
                consolidationDetails.setIsLocked(false);
        } else {
            consolidationDetails.setIsLocked(true);
            consolidationDetails.setLockedBy(currentUser);
        }
        consolidationDetails = consolidationDetailsDao.save(consolidationDetails, false);
        afterSave(consolidationDetails, false);

        return ResponseHelper.buildSuccessResponse();
    }

    private <T extends IRunnerResponse> List<T> getResponse(CompletableFuture<ResponseEntity<?>> responseEntity) throws ExecutionException, InterruptedException {
        var runnerListResponse = (RunnerListResponse<T>) responseEntity.get().getBody();
        return (List<T>) runnerListResponse.getData();
    }

    private <T extends IRunnerResponse> List<T> getResponse(ResponseEntity<?> responseEntity) throws ExecutionException, InterruptedException {
        var runnerListResponse = (RunnerListResponse<T>) responseEntity.getBody();
        return (List<T>) runnerListResponse.getData();
    }

    private <T extends IRunnerResponse> T getResponseEntity(ResponseEntity<?> responseEntity) throws ExecutionException, InterruptedException {
        var runnerResponse = (RunnerResponse<T>) responseEntity.getBody();
        return (T) runnerResponse.getData();
    }

    private Containers convertRequestToEntity(ContainerRequest request) {
        return jsonHelper.convertValue(request, Containers.class);
    }

    @Transactional
    public ResponseEntity<?> completeV1ConsolidationCreateAndUpdate(CommonRequestModel commonRequestModel) throws Exception {
        ConsolidationDetailsRequest consolidationDetailsRequest = (ConsolidationDetailsRequest) commonRequestModel.getData();

        List<PackingRequest> packingRequestList = consolidationDetailsRequest.getPackingList();
        List<ContainerRequest> containerRequestList = consolidationDetailsRequest.getContainersList();
        List<EventsRequest> eventsRequestList = consolidationDetailsRequest.getEventsList();
        List<FileRepoRequest> fileRepoRequestList = consolidationDetailsRequest.getFileRepoList();
        List<JobRequest> jobRequestList = consolidationDetailsRequest.getJobsList();
        List<ReferenceNumbersRequest> referenceNumbersRequestList = consolidationDetailsRequest.getReferenceNumbersList();
        List<RoutingsRequest> routingsRequestList = consolidationDetailsRequest.getRoutingsList();
        List<TruckDriverDetailsRequest> truckDriverDetailsRequestList = consolidationDetailsRequest.getTruckDriverDetails();
        List<PartiesRequest> consolidationAddresses = consolidationDetailsRequest.getConsolidationAddresses();
        CarrierDetailRequest carrierDetailRequest = consolidationDetailsRequest.getCarrierDetails();
        AchievedQuantitiesRequest achievedQuantitiesRequest = consolidationDetailsRequest.getAchievedQuantities();
        AllocationsRequest allocationsRequest = consolidationDetailsRequest.getAllocations();


        UUID guid = consolidationDetailsRequest.getGuid();
        Optional<ConsolidationDetails> oldEntity = null;
        try {
            oldEntity = consolidationDetailsDao.findByGuid(guid);
        }
        catch (Exception e){
        }

        List<ShipmentDetails> tempShipIds = new ArrayList<>();

        List<Long> newShipList = new ArrayList<>();
        List<ShipmentRequest> shipmentRequests = consolidationDetailsRequest.getShipmentsList();
        if(shipmentRequests != null && !shipmentRequests.isEmpty()) {
            for(ShipmentRequest shipmentRequest : shipmentRequests) {
                Optional<ShipmentDetails> shipmentDetails = shipmentDao.findByGuid(shipmentRequest.getGuid());
                if(shipmentDetails.isPresent() && shipmentDetails.get().getId() != null) {
                    ShipmentDetails shipmentDetails1 = new ShipmentDetails();
                    shipmentDetails1.setId(shipmentDetails.get().getId());
                    tempShipIds.add(shipmentDetails1);
                    newShipList.add(shipmentDetails.get().getId());
                }
            }
        }


        try {
            Long id = null;
            ConsolidationDetails oldConsolidation = null;
            boolean isCreate = true;
            if(oldEntity != null && oldEntity.isPresent()) {
                oldConsolidation = oldEntity.get();
                id = oldEntity.get().getId();
                List<Long> oldShipList = oldConsolidation.getShipmentsList().stream().map(e -> e.getId()).collect(Collectors.toList());
                oldShipList = oldShipList.stream().filter(item -> !newShipList.contains(item)).collect(Collectors.toList());
                detachShipmentsReverseSync(oldEntity.get().getId(), oldShipList);
                isCreate = false;
            }
            ConsolidationDetails entity = objectMapper.convertValue(consolidationDetailsRequest, ConsolidationDetails.class);
            entity.setShipmentsList(tempShipIds);
            entity.setId(id);

            if(id == null) {
                entity = consolidationDetailsDao.save(entity, true);
            } else {
                entity = consolidationDetailsDao.update(entity, true);
            }
            // Set id for linking child entitites
            id = entity.getId();

            if(containerRequestList != null) {
                ListCommonRequest listCommonRequest = constructListCommonRequest("consolidationId", entity.getId(), "=");
                Pair<Specification<Containers>, Pageable> containerPair = fetchData(listCommonRequest, Containers.class);
                Page<Containers> oldContainers = containerDao.findAll(containerPair.getLeft(), containerPair.getRight());
                List<Containers> updatedContainers = containerDao.updateEntityFromShipmentConsole(convertToEntityList(containerRequestList, Containers.class), id, oldContainers.stream().toList());
                entity.setContainersList(updatedContainers);
            }
            if (packingRequestList != null) {
                ListCommonRequest listCommonRequest = constructListCommonRequest("consolidationId", entity.getId(), "=");
                Pair<Specification<Packing>, Pageable> packingPair = fetchData(listCommonRequest, Packing.class);
                Page<Packing> oldPackings = packingDao.findAll(packingPair.getLeft(), packingPair.getRight());
                List<Packing> updatedPackings = packingDao.updateEntityFromConsole(convertToEntityList(packingRequestList, Packing.class), id, oldPackings.stream().toList());
                entity.setPackingList(updatedPackings);
            }
            if (eventsRequestList != null) {
                ListCommonRequest listCommonRequest = constructListRequestFromEntityId(entity.getId(), Constants.CONSOLIDATION);
                Pair<Specification<Events>, Pageable> pair = fetchData(listCommonRequest, Events.class);
                Page<Events> oldEvents = eventDao.findAll(pair.getLeft(), pair.getRight());
                List<Events> updatedEvents = eventDao.updateEntityFromOtherEntity(convertToEntityList(eventsRequestList, Events.class), id, Constants.CONSOLIDATION, oldEvents.stream().toList());
                entity.setEventsList(updatedEvents);
            }
            if (fileRepoRequestList != null) {
                ListCommonRequest listCommonRequest = constructListRequestFromEntityId(entity.getId(), Constants.CONSOLIDATION);
                Pair<Specification<FileRepo>, Pageable> pair = fetchData(listCommonRequest, FileRepo.class);
                Page<FileRepo> oldFileRepoList = fileRepoDao.findAll(pair.getLeft(), pair.getRight());
                List<FileRepo> updatedFileRepos = fileRepoDao.updateEntityFromOtherEntity(convertToEntityList(fileRepoRequestList, FileRepo.class), id, Constants.CONSOLIDATION, oldFileRepoList.stream().toList());
                entity.setFileRepoList(updatedFileRepos);
            }
            if (jobRequestList != null) {
                ListCommonRequest listCommonRequest = constructListCommonRequest("consolidationId", entity.getId(), "=");
                Pair<Specification<Jobs>, Pageable> pair = fetchData(listCommonRequest, Jobs.class);
                Page<Jobs> oldJobs = jobDao.findAll(pair.getLeft(), pair.getRight());
                List<Jobs> updatedJobs = jobDao.updateEntityFromConsole(convertToEntityList(jobRequestList, Jobs.class), id, oldJobs.stream().toList());
                entity.setJobsList(updatedJobs);
            }
            if (referenceNumbersRequestList != null) {
                ListCommonRequest listCommonRequest = constructListCommonRequest("consolidationId", entity.getId(), "=");
                Pair<Specification<ReferenceNumbers>, Pageable> pair = fetchData(listCommonRequest, ReferenceNumbers.class);
                Page<ReferenceNumbers> oldReferenceNumbers = referenceNumbersDao.findAll(pair.getLeft(), pair.getRight());
                List<ReferenceNumbers> updatedReferenceNumbers = referenceNumbersDao.updateEntityFromConsole(convertToEntityList(referenceNumbersRequestList, ReferenceNumbers.class), id, oldReferenceNumbers.stream().toList());
                entity.setReferenceNumbersList(updatedReferenceNumbers);
            }
            if (routingsRequestList != null) {
                ListCommonRequest listCommonRequest = constructListCommonRequest("consolidationId", entity.getId(), "=");
                Pair<Specification<Routings>, Pageable> pair = fetchData(listCommonRequest, Routings.class);
                Page<Routings> oldRoutings = routingsDao.findAll(pair.getLeft(), pair.getRight());
                List<Routings> updatedRoutings = routingsDao.updateEntityFromConsole(convertToEntityList(routingsRequestList, Routings.class), id, oldRoutings.stream().toList());
                entity.setRoutingsList(updatedRoutings);
            }
            afterSave(entity, isCreate);
            ConsolidationDetailsResponse response = jsonHelper.convertValue(entity, ConsolidationDetailsResponse.class);

            return ResponseHelper.buildSuccessResponse(response);
        } catch (ExecutionException e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            TransactionAspectSupport.currentTransactionStatus().setRollbackOnly();
            throw new RuntimeException(e);
        }
    }

    private void makeHeadersInSheet(Sheet sheet) {
        Row preHeaderRow = sheet.createRow(0);
        Row headerRow = sheet.createRow(1);
        List<String> consolHeaders = parser.getHeadersForConsolidation();
        for (int i = 0; i < consolHeaders.size(); i++) {
            Cell cell = headerRow.createCell(i);
            cell.setCellValue(consolHeaders.get(i));
        }

        //Create Parties Headers
        int offSet = consolHeaders.size();

        preHeaderRow.createCell(offSet).setCellValue("sendingAgent");
        List<String> sendingAgentParty = parser.getHeadersForParties();
        for (int i = 0; i < sendingAgentParty.size(); i++) {
            Cell cell = headerRow.createCell(offSet + i);
            cell.setCellValue(sendingAgentParty.get(i));
        }
        sheet.addMergedRegion(new CellRangeAddress(0, 0, offSet, offSet + sendingAgentParty.size() - 1));
        offSet += sendingAgentParty.size();

        preHeaderRow.createCell(offSet).setCellValue("receivingAgent");
        List<String> receivingAgentParty = parser.getHeadersForParties();
        for (int i = 0; i < receivingAgentParty.size(); i++) {
            Cell cell = headerRow.createCell(offSet + i);
            cell.setCellValue(receivingAgentParty.get(i));
        }
        sheet.addMergedRegion(new CellRangeAddress(0, 0, offSet, offSet + receivingAgentParty.size() - 1));
        offSet += receivingAgentParty.size();

        preHeaderRow.createCell(offSet).setCellValue("borrowedFrom");
        List<String> borrowedFromParty = parser.getHeadersForParties();
        for (int i = 0; i < borrowedFromParty.size(); i++) {
            Cell cell = headerRow.createCell(offSet + i);
            cell.setCellValue(borrowedFromParty.get(i));
        }
        sheet.addMergedRegion(new CellRangeAddress(0, 0, offSet, offSet + borrowedFromParty.size() - 1));
        offSet += borrowedFromParty.size();

        preHeaderRow.createCell(offSet).setCellValue("creditor");
        List<String> creditorParty = parser.getHeadersForParties();
        for (int i = 0; i < creditorParty.size(); i++) {
            Cell cell = headerRow.createCell(offSet + i);
            cell.setCellValue(creditorParty.get(i));
        }
        sheet.addMergedRegion(new CellRangeAddress(0, 0, offSet, offSet + creditorParty.size() - 1));
        offSet += creditorParty.size();

        preHeaderRow.createCell(offSet).setCellValue("coLoadWith");
        List<String> coLoadWith = parser.getHeadersForParties();
        for (int i = 0; i < coLoadWith.size(); i++) {
            Cell cell = headerRow.createCell(offSet + i);
            cell.setCellValue(coLoadWith.get(i));
        }
        sheet.addMergedRegion(new CellRangeAddress(0, 0, offSet, offSet + coLoadWith.size() - 1));
        offSet += coLoadWith.size();

        preHeaderRow.createCell(offSet).setCellValue("arrivalDetails");
        List<String> arrivalDetails = parser.getHeadersForArrivalDepartureDetails();
        for (int i = 0; i < arrivalDetails.size(); i++) {
            Cell cell = headerRow.createCell(offSet + i);
            cell.setCellValue(arrivalDetails.get(i));
        }
        sheet.addMergedRegion(new CellRangeAddress(0, 0, offSet, offSet + arrivalDetails.size() - 1));
        offSet += arrivalDetails.size();

        preHeaderRow.createCell(offSet).setCellValue("departureDetails");
        List<String> departureDetails = parser.getHeadersForArrivalDepartureDetails();
        for (int i = 0; i < departureDetails.size(); i++) {
            Cell cell = headerRow.createCell(offSet + i);
            cell.setCellValue(departureDetails.get(i));
        }
        sheet.addMergedRegion(new CellRangeAddress(0, 0, offSet, offSet + departureDetails.size() - 1));
        offSet += departureDetails.size();


        preHeaderRow.createCell(offSet).setCellValue("allocations");
        List<String> allocations = parser.getHeadersForAllocations();
        for (int i = 0; i < allocations.size(); i++) {
            Cell cell = headerRow.createCell(offSet + i);
            cell.setCellValue(allocations.get(i));
        }
        sheet.addMergedRegion(new CellRangeAddress(0, 0, offSet, offSet + allocations.size() - 1));
        offSet += allocations.size();

        preHeaderRow.createCell(offSet).setCellValue("Carrier Details");
        List<String> carrierDetails = parser.getHeadersForCarrier();
        for (int i = 0; i < carrierDetails.size(); i++) {
            Cell cell = headerRow.createCell(offSet + i);
            cell.setCellValue(carrierDetails.get(i));
        }
        sheet.addMergedRegion(new CellRangeAddress(0, 0, offSet, offSet + carrierDetails.size() - 1));
        offSet += carrierDetails.size();

        preHeaderRow.createCell(offSet).setCellValue("Achieved Quantities");
        List<String> achievedQuantities = parser.getHeadersForAchievedQuantities();
        for (int i = 0; i < achievedQuantities.size(); i++) {
            Cell cell = headerRow.createCell(offSet + i);
            cell.setCellValue(achievedQuantities.get(i));
        }
        sheet.addMergedRegion(new CellRangeAddress(0, 0, offSet, offSet + achievedQuantities.size() - 1));
        offSet += achievedQuantities.size();
    }

    @Override
    public void exportExcel(HttpServletResponse response, CommonRequestModel commonRequestModel) throws IOException, IllegalAccessException {
        String responseMsg;

        ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
        if (request == null) {
            log.error("Request is empty for Consolidation list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        Pair<Specification<ConsolidationDetails>, Pageable> tuple = fetchData(request, ConsolidationDetails.class, tableNames);
        Page<ConsolidationDetails> consolidationDetailsPage = consolidationDetailsDao.findAll(tuple.getLeft(), tuple.getRight());
        List<IRunnerResponse> consoleResponse = convertEntityListToDtoList(consolidationDetailsPage.getContent());
        log.info("Consolidation list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());

        Workbook workbook = new XSSFWorkbook();
        Sheet sheet = workbook.createSheet("ConsolidationList");
        makeHeadersInSheet(sheet);

        for (int i = 0; i < consoleResponse.size(); i++) {
            Row itemRow = sheet.createRow(i + 2);
            ConsolidationListResponse consol = (ConsolidationListResponse) consoleResponse.get(i);
            var consolBasicValues = parser.getAllAttributeValuesAsListConsol(consol);
            int offset = 0;
            for (int j = 0; j < consolBasicValues.size(); j++)
                itemRow.createCell(offset + j).setCellValue(consolBasicValues.get(j));
            offset += consolBasicValues.size();

            var sendingAgentValues = parser.getAllAttributeValuesAsListForParty(consol.getSendingAgent());
            for (int j = 0; j < sendingAgentValues.size(); j++)
                itemRow.createCell(offset + j).setCellValue(sendingAgentValues.get(j));
            offset += sendingAgentValues.size();

            var receivingAgentValues = parser.getAllAttributeValuesAsListForParty(consol.getReceivingAgent());
            for (int j = 0; j < receivingAgentValues.size(); j++)
                itemRow.createCell(offset + j).setCellValue(receivingAgentValues.get(j));
            offset += receivingAgentValues.size();

            var borrowedFrom = parser.getAllAttributeValuesAsListForParty(consol.getBorrowedFrom());
            for (int j = 0; j < borrowedFrom.size(); j++)
                itemRow.createCell(offset + j).setCellValue(borrowedFrom.get(j));
            offset += borrowedFrom.size();

            var creditor = parser.getAllAttributeValuesAsListForParty(consol.getCreditor());
            for (int j = 0; j < creditor.size(); j++)
                itemRow.createCell(offset + j).setCellValue(creditor.get(j));
            offset += creditor.size();

            var coLoadWith = parser.getAllAttributeValuesAsListForParty(consol.getCoLoadWith());
            for (int j = 0; j < coLoadWith.size(); j++)
                itemRow.createCell(offset + j).setCellValue(coLoadWith.get(j));
            offset += coLoadWith.size();

            var arrivalDetails = parser.getAllAttributeValuesAsListForArrivalDepartureDetails(consol.getArrivalDetails());
            for (int j = 0; j < arrivalDetails.size(); j++)
                itemRow.createCell(offset + j).setCellValue(arrivalDetails.get(j));
            offset += arrivalDetails.size();

            var departureDetails = parser.getAllAttributeValuesAsListForArrivalDepartureDetails(consol.getDepartureDetails());
            for (int j = 0; j < departureDetails.size(); j++)
                itemRow.createCell(offset + j).setCellValue(departureDetails.get(j));
            offset += departureDetails.size();

            var allocations = parser.getAllAttributeValuesAsListForAllocations(consol.getAllocations());
            for (int j = 0; j < allocations.size(); j++)
                itemRow.createCell(offset + j).setCellValue(allocations.get(j));
            offset += allocations.size();

            var carrierDetails = parser.getAllAttributeValuesAsListForCarrier(consol.getCarrierDetails());
            for (int j = 0; j < carrierDetails.size(); j++)
                itemRow.createCell(offset + j).setCellValue(carrierDetails.get(j));
            offset += carrierDetails.size();

            var achievedQuantities = parser.getAllAttributeValuesAsListForAchievedQuantities(consol.getAchievedQuantities());
            for (int j = 0; j < achievedQuantities.size(); j++)
                itemRow.createCell(offset + j).setCellValue(achievedQuantities.get(j));
            offset += achievedQuantities.size();
        }

        response.setContentType("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet");
        response.setHeader("Content-Disposition", "attachment; filename=consolidationList.xlsx");

        try (OutputStream outputStream = response.getOutputStream()) {
            workbook.write(outputStream);
        }

    }

    @Override
    public void afterSave(ConsolidationDetails consolidationDetails, boolean isCreate)
    {
        try {
            if(trackingServiceAdapter.checkIfConsolContainersExist(consolidationDetails) || trackingServiceAdapter.checkIfAwbExists(consolidationDetails)) {
                UniversalTrackingPayload _utPayload = trackingServiceAdapter.mapConsoleDataToTrackingServiceData(consolidationDetails);
                List<UniversalTrackingPayload> trackingPayloads = new ArrayList<>();
                if(_utPayload != null) {
                    trackingPayloads.add(_utPayload);
                    var jsonBody = jsonHelper.convertToJson(trackingPayloads);
                    trackingServiceAdapter.publishUpdatesToTrackingServiceQueue(jsonBody, false);
                }
            }
            if(consolidationDetails != null) {
                var events = trackingServiceAdapter.getAllEvents(null,consolidationDetails);
                var universalEventsPayload = trackingServiceAdapter.mapEventDetailsForTracking(consolidationDetails.getReferenceNumber(),Constants.CONSOLIDATION, consolidationDetails.getConsolidationNumber(), events);
                List<UniversalTrackingPayload.UniversalEventsPayload> trackingPayloads= new ArrayList<>();
                if(universalEventsPayload != null) {
                    trackingPayloads.add(universalEventsPayload);
                    var jsonBody = jsonHelper.convertToJson(trackingPayloads);
                    trackingServiceAdapter.publishUpdatesToTrackingServiceQueue(jsonBody,true);
                }
            }
            KafkaResponse kafkaResponse = producer.getKafkaResponse(consolidationDetails, isCreate);
            producer.produceToKafka(jsonHelper.convertToJson(kafkaResponse), senderQueue, UUID.randomUUID().toString());
        }
        catch (Exception e) {
            log.error(e.getMessage());
        }
    }
}
