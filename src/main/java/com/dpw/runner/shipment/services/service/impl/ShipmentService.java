package com.dpw.runner.shipment.services.service.impl;


import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.*;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dao.impl.*;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.request.ShipmentRequest;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.GenerationType;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.repository.interfaces.*;
import com.dpw.runner.shipment.services.service.interfaces.*;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.JoinPoint;
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
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;
import static com.dpw.runner.shipment.services.utils.CommonUtils.convertToClass;

@SuppressWarnings("ALL")
@Service
@Slf4j
public class ShipmentService implements IShipmentService {

    @Autowired
    private ModelMapper modelMapper;

    @Autowired
    private IShipmentDao shipmentDao;
    @Autowired
    private ICarrierDao carrierDao;
    @Autowired
    private IPartiesDao partiesDao;

    @Autowired
    private AdditionalDetailDao additionalDetailDao;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private PackingDao packingDao;

    @Autowired
    private BookingCarriageDao bookingCarriageDao;

    @Autowired
    private ELDetailsDao elDetailsDao;

    @Autowired
    private EventDao eventDao;

    @Autowired
    private FileRepoDao fileRepoDao;

    @Autowired
    private JobDao jobDao;

    @Autowired
    private NotesDao notesDao;

    @Autowired
    private PickupDeliveryDetailsDao pickupDeliveryDetailsDao;

    @Autowired
    private ReferenceNumbersDao referenceNumbersDao;

    @Autowired
    private RoutingsDao routingsDao;

    @Autowired
    private ServiceDetailsDao serviceDetailsDao;

    @Autowired
    private ContainerDao containerDao;

    @Autowired
    private TransactionTemplate transactionTemplate;

    @Autowired
    private IShipmentSettingsDao shipmentSettingsDao;

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
            Map.entry("orgCode", RunnerEntityMapping.builder().tableName("parties").dataType(Integer.class).build()),
            Map.entry("houseBill", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("hblType", RunnerEntityMapping.builder().tableName("additionalDetails").dataType(String.class).build()),
            Map.entry("transportMode", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("releaseType", RunnerEntityMapping.builder().tableName("additionalDetails").dataType(String.class).build()),
            Map.entry("deliveryMode", RunnerEntityMapping.builder().tableName("additionalDetails").dataType(String.class).build()),
            Map.entry("direction", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("shipmentType", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("status", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(Integer.class).build()),
            Map.entry("source", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("jobType", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("serviceType", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("masterBill", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("bookingReference", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("consolRef", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("salesAgent", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(Long.class).build()),
            Map.entry("paymentTerms", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("incoterms", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("shipmentId", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("isDomestic", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(Boolean.class).build()),
            Map.entry("assignedTo", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(Integer.class).build()),
            Map.entry("additionalTerms", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("goodsDescription", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("createdAt", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(Date.class).build()),
            Map.entry("estimatedPickupOrDelivery", RunnerEntityMapping.builder().tableName("pickupDeliveryDetails").dataType(LocalDateTime.class).build()),
            Map.entry("actualPickupOrDelivery", RunnerEntityMapping.builder().tableName("pickupDeliveryDetails").dataType(LocalDateTime.class).build()),
            Map.entry("requiredBy", RunnerEntityMapping.builder().tableName("pickupDeliveryDetails").dataType(LocalDateTime.class).build()),
            Map.entry("addressCode", RunnerEntityMapping.builder().tableName("parties").dataType(Integer.class).build()),
            Map.entry("screeningStatus", RunnerEntityMapping.builder().tableName("additionalDetails").dataType(String.class).build()),
            Map.entry("paidPlace", RunnerEntityMapping.builder().tableName("additionalDetails").dataType(Long.class).build()),
            Map.entry("placeOfIssue", RunnerEntityMapping.builder().tableName("additionalDetails").dataType(Long.class).build()),
            Map.entry("dateOfIssue", RunnerEntityMapping.builder().tableName("additionalDetails").dataType(LocalDateTime.class).build()),
            Map.entry("dateOfReceipt", RunnerEntityMapping.builder().tableName("additionalDetails").dataType(LocalDateTime.class).build()),
            Map.entry("goodsCo", RunnerEntityMapping.builder().tableName("additionalDetails").dataType(String.class).build()),
            Map.entry("boeDate", RunnerEntityMapping.builder().tableName("additionalDetails").dataType(LocalDateTime.class).build()),
            Map.entry("boeNumber", RunnerEntityMapping.builder().tableName("additionalDetails").dataType(String.class).build()),
            Map.entry("shippingLine", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(String.class).build()),
            Map.entry("vessel", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(String.class).build()),
            Map.entry("voyage", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(String.class).build()),
            Map.entry("origin", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(String.class).build()),
            Map.entry("destination", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(String.class).build()),
            Map.entry("eta", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(LocalDateTime.class).build()),
            Map.entry("etd", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(LocalDateTime.class).build()),
            Map.entry("ata", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(LocalDateTime.class).build()),
            Map.entry("weight", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(BigDecimal.class).build()),
            Map.entry("weightUnit", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("volume", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(BigDecimal.class).build()),
            Map.entry("volumeUnit", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("volumetricWeight", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(BigDecimal.class).build()),
            Map.entry("volumetricWeightUnit", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("chargable", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(BigDecimal.class).build()),
            Map.entry("chargeableUnit", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("netWeight", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(BigDecimal.class).build()),
            Map.entry("netWeightUnit", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("noOfPacks", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(Integer.class).build()),
            Map.entry("packsUnit", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("innerPacks", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(Integer.class).build()),
            Map.entry("innerPackUnit", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("containerNumber", RunnerEntityMapping.builder().tableName("containers").dataType(String.class).build()),
            Map.entry("containerCode", RunnerEntityMapping.builder().tableName("containers").dataType(String.class).build())
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
    public ResponseEntity<?> fetchShipments(CommonRequestModel commonRequestModel) {
        ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();

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
            responseList.add(jsonHelper.convertValue(shipmentDetail, ShipmentDetailsResponse.class));
        });
        return responseList;
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
        System.out.println(jsonHelper.convertToJson(request));
        ShipmentDetails shipmentDetails = jsonHelper.convertValue(request, ShipmentDetails.class);
        AdditionalDetail additionalDetail = jsonHelper.convertValue(request.getAdditionalDetail(), AdditionalDetail.class);
        CarrierDetails carrierDetails = jsonHelper.convertValue(request.getCarrierDetails(), CarrierDetails.class);

        try {

            if(additionalDetail != null) {
                createAdditionalDetail(shipmentDetails, additionalDetail);
                shipmentDetails.setAdditionalDetail(additionalDetail);
            }

            if(carrierDetails != null) {
                createCarrier(shipmentDetails, carrierDetails);
                shipmentDetails.setCarrierDetails(carrierDetails);
            }

            List<ContainerRequest> containerRequest = request.getContainersList();
            List<Containers> containers = createContainersAsync(shipmentDetails, containerRequest);
            shipmentDetails.setContainers(containers);
            getShipment(shipmentDetails);



            List<PackingRequest> packingRequest = request.getPackingList();
            if(packingRequest != null)
                createPackingsAsync(shipmentDetails, packingRequest);


            List<BookingCarriageRequest> bookingCarriageRequest = request.getBookingCarriagesList();
            if(bookingCarriageRequest != null)
                createBookingCarriagesAsync(shipmentDetails, bookingCarriageRequest);


            List<ELDetailsRequest> elDetailsRequest = request.getElDetailsList();
            if(elDetailsRequest != null)
                createElDetailsAsync(shipmentDetails, elDetailsRequest);


            List<EventsRequest> eventsRequest = request.getEventsList();
            if(eventsRequest != null)
                createEventsAsync(shipmentDetails, eventsRequest);


            List<FileRepoRequest> fileRepoRequest = request.getFileRepoList();
            if(fileRepoRequest != null)
                createFileRepoAsync(shipmentDetails, fileRepoRequest);


            List<JobRequest> jobRequest = request.getJobsList();
            if(jobRequest != null)
                createJobsAsync(shipmentDetails, jobRequest);


            List<NotesRequest> notesRequest = request.getNotesList();
            if(notesRequest != null)
                createNotesAsync(shipmentDetails, notesRequest);


            List<ReferenceNumbersRequest> referenceNumbersRequest = request.getReferenceNumbersList();
            if(referenceNumbersRequest != null)
                createReferenceNumbersAsync(shipmentDetails, referenceNumbersRequest);


            List<RoutingsRequest> routingsRequest = request.getRoutingsList();
            if(routingsRequest != null)
                createRoutingsAsync(shipmentDetails, routingsRequest);


            List<ServiceDetailsRequest> serviceDetailsRequest = request.getServicesList();
            if(serviceDetailsRequest != null)
                createServiceDetailsAsync(shipmentDetails, serviceDetailsRequest);


            List<PickupDeliveryDetailsRequest> pickupDeliveryDetailsRequest = request.getPickupDeliveryDetailsList();
            if(pickupDeliveryDetailsRequest != null)
                createPickupDeliveryAsync(shipmentDetails, pickupDeliveryDetailsRequest);

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
        shipmentDao.save(shipmentDetails);
    }


    private void createPartiesAsync(ShipmentDetails shipmentDetails, List<PartiesRequest> partiesRequest) {
        partiesRequest.forEach(parties -> {
            createParties(shipmentDetails, parties);
        });
    }

    private void createPickupDeliveryAsync(ShipmentDetails shipmentDetails, List<PickupDeliveryDetailsRequest> pickupDeliveryDetailsRequest) {
        pickupDeliveryDetailsRequest.forEach(pickupDelivery -> {
            createPickupDelivery(shipmentDetails, pickupDelivery);
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

    private List<Containers> createContainersAsync(ShipmentDetails shipmentDetails, List<ContainerRequest> containerRequest) {
        return containerDao.saveAll(containerRequest.stream().map(e -> modelMapper.map(e, Containers.class)).collect(Collectors.toList()));
    }

    @Transactional
    public void createbookingCarriage(ShipmentDetails shipmentDetails, BookingCarriageRequest bookingCarriageRequest) {
        bookingCarriageRequest.setShipmentId(shipmentDetails.getId());
        bookingCarriageDao.save(modelMapper.map(bookingCarriageRequest, BookingCarriage.class));
    }

    @Transactional
    public void createElDetail(ShipmentDetails shipmentDetails, ELDetailsRequest elDetailsRequest) {
        elDetailsRequest.setShipmentId(shipmentDetails.getId());
        elDetailsDao.save(modelMapper.map(elDetailsRequest, ELDetails.class));
    }

    @Transactional
    public void createEvent(ShipmentDetails shipmentDetails, EventsRequest eventsRequest) {
        eventsRequest.setShipmentId(shipmentDetails.getId());
        eventDao.save(modelMapper.map(eventsRequest, Events.class));
    }

    @Transactional
    public void createFileRepo(ShipmentDetails shipmentDetails, FileRepoRequest fileRepoRequest) {
        fileRepoRequest.setEntityId(shipmentDetails.getId());
        fileRepoRequest.setEntityType(Constants.SHIPMENT);
        fileRepoDao.save(modelMapper.map(fileRepoRequest, FileRepo.class));
    }

    @Transactional
    public void createJob(ShipmentDetails shipmentDetails, JobRequest jobRequest) {
        jobRequest.setShipmentId(shipmentDetails.getId());
        jobDao.save(modelMapper.map(jobRequest, Jobs.class));
    }

    @Transactional
    public void createNote(ShipmentDetails shipmentDetails, NotesRequest notesRequest) {
        notesRequest.setEntityId(shipmentDetails.getId());
        notesRequest.setEntityType(Constants.SHIPMENT);
        notesDao.save(modelMapper.map(notesRequest, Notes.class));
    }

    @Transactional
    public void createParties(ShipmentDetails shipmentDetails, PartiesRequest partiesRequest) {
        partiesRequest.setEntityId(shipmentDetails.getId());
        partiesRequest.setEntityType("SHIPMENT");
        packingDao.save(modelMapper.map(partiesRequest, Packing.class));
    }

    @Transactional
    public void createPickupDelivery(ShipmentDetails shipmentDetails, PickupDeliveryDetailsRequest pickupDeliveryDetailsRequest) {
        pickupDeliveryDetailsRequest.setShipmentId(shipmentDetails.getId());
        pickupDeliveryDetailsDao.save(modelMapper.map(pickupDeliveryDetailsRequest, PickupDeliveryDetails.class));
    }

    @Transactional
    public void createReferenceNumber(ShipmentDetails shipmentDetails, ReferenceNumbersRequest referenceNumbersRequest) {
        referenceNumbersRequest.setShipmentId(shipmentDetails.getId());
        referenceNumbersDao.save(modelMapper.map(referenceNumbersRequest, ReferenceNumbers.class));
    }

    @Transactional
    public void createPacking(ShipmentDetails shipmentDetails, PackingRequest packingRequest) {
        packingRequest.setShipmentId(shipmentDetails.getId());
        packingDao.save(modelMapper.map(packingRequest, Packing.class));
    }

    @Transactional
    public void createRouting(ShipmentDetails shipmentDetails, RoutingsRequest routingsRequest) {
        routingsRequest.setShipmentId(shipmentDetails.getId());
        routingsDao.save(modelMapper.map(routingsRequest, Routings.class));
    }

    @Transactional
    public void createServiceDetail(ShipmentDetails shipmentDetails, ServiceDetailsRequest serviceDetailsRequest) {
        serviceDetailsRequest.setShipmentId(shipmentDetails.getId());
        serviceDetailsDao.save(modelMapper.map(serviceDetailsRequest, ServiceDetails.class));
    }

    @Transactional
    public void createAdditionalDetail(ShipmentDetails shipmentDetails, AdditionalDetail additionalDetail) {
        additionalDetailDao.save(additionalDetail);
    }

    @Transactional
    public void createCarrier(ShipmentDetails shipmentDetails, CarrierDetails carrierDetails) {
        carrierDao.save(carrierDetails);
    }

    @Override
    @Transactional
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) {
        ShipmentRequest request = (ShipmentRequest) commonRequestModel.getData();
        // TODO- implement Validation logic
        long id = request.getId();
        Optional<ShipmentDetails> oldEntity = shipmentDao.findById(id);
        if (!oldEntity.isPresent()) {
            log.debug("Shipment Details is null for Id {}", request.getId());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        ShipmentDetails entity = modelMapper.map(request, ShipmentDetails.class);
        entity.setId(oldEntity.get().getId());
        if(entity.getContainers() == null)
            entity.setContainers(oldEntity.get().getContainers());
        entity = shipmentDao.save(entity);
        return ResponseHelper.buildSuccessResponse(modelMapper.map(entity, ShipmentDetailsResponse.class));
    }

    @Transactional
    public ResponseEntity<?> completeUpdate(CommonRequestModel commonRequestModel) throws Exception {

        ShipmentRequest shipmentRequest = (ShipmentRequest) commonRequestModel.getData();

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
        List<PickupDeliveryDetailsRequest> pickupDeliveryDetailsRequestList = shipmentRequest.getPickupDeliveryDetailsList();

        // TODO- implement Validation logic
        long id = shipmentRequest.getId();
        Optional<ShipmentDetails> oldEntity = shipmentDao.findById(id);
        if (!oldEntity.isPresent()) {
            log.debug("Shipment Details is null for Id {}", shipmentRequest.getId());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        try {

            ShipmentDetails entity = modelMapper.map(shipmentRequest, ShipmentDetails.class);
            entity.setId(oldEntity.get().getId());
            List<Containers> updatedContainers = null;
            if(containerRequestList != null) {
                updatedContainers = containerDao.updateEntityFromShipment(CommonRequestModel.buildRequest(containerRequestList));
            }
            else {
                updatedContainers = oldEntity.get().getContainers();
            }
            entity.setContainers(updatedContainers);
            AdditionalDetail updatedAdditionalDetails = null;
            if(additionalDetailRequest != null) {
                updatedAdditionalDetails = additionalDetailDao.updateEntityFromShipment(CommonRequestModel.buildRequest(additionalDetailRequest), id);
                entity.setAdditionalDetail(updatedAdditionalDetails);
            }
            ResponseEntity<?> updatedCarrierDetails = null;
            if(carrierDetailRequest != null) {
                updatedCarrierDetails = carrierDetailService.updateEntityFromShipment(CommonRequestModel.buildRequest(carrierDetailRequest), id);
                entity.setCarrierDetails(modelMapper.map(getResponseEntity(updatedCarrierDetails), CarrierDetails.class));
            }
            entity = shipmentDao.save(entity);

            ShipmentDetailsResponse response = modelMapper.map(entity, ShipmentDetailsResponse.class);
            response.setContainersList(updatedContainers.stream().map(e -> modelMapper.map(e, ContainerResponse.class)).collect(Collectors.toList()));
            if(additionalDetailRequest != null) {
                response.setAdditionalDetail(convertToClass(updatedAdditionalDetails, AdditionalDetailResponse.class));
            }
            if(carrierDetailRequest != null) {
                response.setCarrierDetails(getResponseEntity(updatedCarrierDetails));
            }
            if(bookingCarriageRequestList != null) {
                List<BookingCarriage> updatedBookingCarriages = bookingCarriageDao.updateEntityFromShipment(CommonRequestModel.buildRequest(bookingCarriageRequestList), id);
                response.setBookingCarriagesList(updatedBookingCarriages.stream().map(e -> convertToClass(e, BookingCarriageResponse.class)).collect(Collectors.toList()));
            }
            if(packingRequestList != null) {
                List<Packing> updatedPackings = packingDao.updateEntityFromShipment(CommonRequestModel.buildRequest(packingRequestList), id);
                response.setPackingList(updatedPackings.stream().map(e -> convertToClass(e, PackingResponse.class)).collect(Collectors.toList()));
            }
            if(elDetailsRequestList != null) {
                List<ELDetails> updatedELDetails = elDetailsDao.updateEntityFromShipment(CommonRequestModel.buildRequest(elDetailsRequestList), id);
                response.setElDetailsList(updatedELDetails.stream().map(e -> convertToClass(e, ELDetailsResponse.class)).collect(Collectors.toList()));
            }
            if(eventsRequestList != null) {
                List<Events> updatedEvents = eventDao.updateEntityFromShipment(CommonRequestModel.buildRequest(eventsRequestList), id);
                response.setEventsList(updatedEvents.stream().map(e -> convertToClass(e, EventsResponse.class)).collect(Collectors.toList()));
            }
            if(fileRepoRequestList != null) {
                List<FileRepo> updatedFileRepos = fileRepoDao.updateEntityFromShipment(CommonRequestModel.buildRequest(fileRepoRequestList), id);
                response.setFileRepoList(updatedFileRepos.stream().map(e -> convertToClass(e, FileRepoResponse.class)).collect(Collectors.toList()));
            }
            if(jobRequestList != null) {
                ResponseEntity<?> updatedJobs = jobService.updateEntityFromShipment(CommonRequestModel.buildRequest(jobRequestList), id);
                response.setJobsList(getResponse(updatedJobs));
            }
            if(notesRequestList != null) {
                ResponseEntity<?> updatedNotes = notesService.updateEntityFromShipment(CommonRequestModel.buildRequest(notesRequestList), id);
                response.setNotesList(getResponse(updatedNotes));
            }
            if(referenceNumbersRequestList != null) {
                ResponseEntity<?> updatedReferenceNumbers = referenceNumbersService.updateEntityFromShipment(CommonRequestModel.buildRequest(referenceNumbersRequestList), id);
                response.setReferenceNumbersList(getResponse(updatedReferenceNumbers));
            }
            if(routingsRequestList != null) {
                ResponseEntity<?> updatedRoutings = routingsService.updateEntityFromShipment(CommonRequestModel.buildRequest(routingsRequestList), id);
                response.setRoutingsList(getResponse(updatedRoutings));
            }
            if(serviceDetailsRequestList != null) {
                ResponseEntity<?> updatedServiceDetails = serviceDetailsService.updateEntityFromShipment(CommonRequestModel.buildRequest(serviceDetailsRequestList), id);
                response.setServicesList(getResponse(updatedServiceDetails));
            }
            if(pickupDeliveryDetailsRequestList != null) {
                ResponseEntity<?> updatedPickupDeliveryDetails = pickupDeliveryDetailsService.updateEntityFromShipment(CommonRequestModel.buildRequest(pickupDeliveryDetailsRequestList), id);
                response.setPickupDeliveryDetailsList(getResponse(updatedPickupDeliveryDetails));
            }

            return ResponseHelper.buildSuccessResponse(response);
        } catch (ExecutionException e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public ResponseEntity<?> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            // TODO- implement actual logic with filters
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();

            Pair<Specification<ShipmentDetails>, Pageable> tuple = fetchData(request, ShipmentDetails.class, tableNames);
            Page<ShipmentDetails> shipmentDetailsPage = shipmentDao.findAll(tuple.getLeft(), tuple.getRight());
            return ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(shipmentDetailsPage.getContent()),
                    shipmentDetailsPage.getTotalPages(),
                    shipmentDetailsPage.getTotalElements());
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

            Pair<Specification<ShipmentDetails>, Pageable> tuple = fetchData(request, ShipmentDetails.class, tableNames);
            Page<ShipmentDetails> shipmentDetailsPage = shipmentDao.findAll(tuple.getLeft(), tuple.getRight());
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
            long id = request.getId();
            Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(id);
            if (!shipmentDetails.isPresent()) {
                log.debug("Shipment Details is null for Id {}", request.getId());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            shipmentDao.delete(shipmentDetails.get());
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
            long id = request.getId();
            Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(id);
            if (!shipmentDetails.isPresent()) {
                log.debug("Shipment Details is null for Id {}", request.getId());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }

            ShipmentDetailsResponse response = jsonHelper.convertValue(shipmentDetails.get(), ShipmentDetailsResponse.class);
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
            long id = request.getId();
            Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(id);
            if (!shipmentDetails.isPresent()) {
                log.debug("Shipment Details is null for Id {}", request.getId());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }

            ShipmentDetailsResponse response = jsonHelper.convertValue(shipmentDetails.get(), ShipmentDetailsResponse.class);
            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(response));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return CompletableFuture.completedFuture(ResponseHelper.buildFailedResponse(responseMsg));
        }
    }

    public ResponseEntity<?> completeRetrieveById(CommonRequestModel commonRequestModel) throws ExecutionException, InterruptedException {
        try{
        // create common list request for shipment id
        CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
        long id = request.getId();
        CommonRequestModel commonListRequestModel = CommonRequestModel.buildRequest(constructListCommonRequest("shipmentId",id,"=" ));
        CommonRequestModel commonListRequestModelbyEntityId = CommonRequestModel.buildRequest(constructListCommonRequest("entityId",id,"=" ));

        CompletableFuture<ResponseEntity<?>> shipmentsFuture = retrieveByIdAsync(commonRequestModel);
        var res = (RunnerResponse<ShipmentDetailsResponse>) shipmentsFuture.get().getBody();

        return ResponseHelper.buildSuccessResponse(res.getData());
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
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

    private String generateShipmentId() {
        List<ShipmentSettingsDetails> shipmentSettingsList = shipmentSettingsDao.list();
        if (shipmentSettingsList.isEmpty())
            return StringUtility.getRandomString(10);
        return createShipmentSequence(shipmentSettingsList.get(0));
    }

    private String createShipmentSequence(ShipmentSettingsDetails shipmentSetting) {
        String sequence = generateSequence(shipmentSetting.getShipmentIdGenerationType(), shipmentSetting.getShipmentIdGenerationPrefix(), shipmentSetting.getShipmentIdGenerationCounter());
        if (shipmentSetting.getShipmentIdGenerationType() == GenerationType.SERIAL) {
            shipmentSetting.setShipmentIdGenerationCounter(shipmentSetting.getShipmentIdGenerationCounter() + 1);
            shipmentSettingsDao.save(shipmentSetting);
        }
        return sequence;
    }

    private String generateSequence(GenerationType generationType, String prefix, Integer counter) {
        String suffix;
        switch (generationType) {
            case RANDOM:
                suffix = StringUtility.getRandomString(10);
                break;
            case SERIAL:
                suffix = String.valueOf(counter);
                break;
            default:
                suffix = StringUtility.getEmptyString();
        }
        return !StringUtils.isEmpty(prefix) ? prefix + suffix : suffix;
    }
}
