package com.dpw.runner.shipment.services.service.impl;


import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.*;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.ShipmentRequest;
import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.repository.interfaces.*;
import com.dpw.runner.shipment.services.service.interfaces.*;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.JoinPoint;
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

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.stream.Collectors;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@SuppressWarnings("ALL")
@Service
@Slf4j
public class ShipmentService implements IShipmentService {

    @Autowired
    private IShipmentDao shipmentDao;

    @Autowired
    private IPackingService packingService;

    @Autowired
    private IBookingCarriageService bookingCarriageService;

    @Autowired
    private ICarrierDetailService carrierDetailService;

    @Autowired
    private IELDetailsService elDetailsService;

    @Autowired
    private IEventService eventService;

    @Autowired
    private IFileRepoService fileRepoService;

    @Autowired
    private IJobService jobService;

    @Autowired
    private INotesService notesService;

    @Autowired
    private IPickupDeliveryDetailsService pickupDeliveryDetailsService;

    @Autowired
    private IReferenceNumbersService referenceNumbersService;

    @Autowired
    private IRoutingsService routingsService;

    @Autowired
    private IServiceDetailsService serviceDetailsService;

    @Autowired
    private IAdditionalDetailService additionalDetailService;

    @Autowired
    private IContainerService containerService;

    @Autowired
    private IPartiesDetailsService partiesDetailsService;

    @Autowired
    private IBlDetailsDao blDetailsDao;

    @Autowired
    private IMeasurementDao measurementDao;

    @Autowired
    private ICarrierDao carrierDao;

    @Autowired
    private IPartiesDao partiesDao;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private TransactionTemplate transactionTemplate;

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
            Map.entry("orgId", RunnerEntityMapping.builder().tableName("parties").dataType(Integer.class).build()),
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
            Map.entry("estimatedPickup", RunnerEntityMapping.builder().tableName("pickupDetails").dataType(LocalDateTime.class).build()),
            Map.entry("actualPickup", RunnerEntityMapping.builder().tableName("pickupDetails").dataType(LocalDateTime.class).build()),
            Map.entry("estimatedDelivery", RunnerEntityMapping.builder().tableName("deliveryDetails").dataType(LocalDateTime.class).build()),
            Map.entry("requiredBy", RunnerEntityMapping.builder().tableName("deliveryDetails").dataType(LocalDateTime.class).build()),
            Map.entry("addressId", RunnerEntityMapping.builder().tableName("parties").dataType(Integer.class).build()),
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

    private static ShipmentDetailsResponse convertEntityToDto(ShipmentDetails shipmentDetails) {
        ShipmentDetailsResponse response = new ShipmentDetailsResponse();
        response.setId(shipmentDetails.getId());
        response.setGuid(shipmentDetails.getGuid());
        response.setParties(shipmentDetails.getParties());
        response.setBlDetails(shipmentDetails.getBlDetails());
        response.setDeliveryDetails(shipmentDetails.getDeliveryDetails());
        response.setPickupDetails(shipmentDetails.getPickupDetails());
        response.setGoodsDescription(shipmentDetails.getGoodsDescription());
        response.setAdditionalTerms(shipmentDetails.getAdditionalTerms());
        response.setAssignedTo(shipmentDetails.getAssignedTo());
        response.setIsDomestic(shipmentDetails.getIsDomestic());
        response.setShipmentId(shipmentDetails.getShipmentId());
        response.setIncoterms(shipmentDetails.getIncoterms());
        response.setPaymentTerms(shipmentDetails.getPaymentTerms());
        response.setSalesAgent(shipmentDetails.getSalesAgent());
        response.setConsolRef(shipmentDetails.getConsolRef());
        response.setBookingReference(shipmentDetails.getBookingReference());
        response.setMasterBill(shipmentDetails.getMasterBill());
        response.setServiceType(shipmentDetails.getServiceType());
        response.setJobType(shipmentDetails.getJobType());
        response.setSource(shipmentDetails.getSource());
        response.setStatus(shipmentDetails.getStatus());
        response.setShipmentType(shipmentDetails.getShipmentType());
        response.setDirection(shipmentDetails.getDirection());
        response.setTransportMode(shipmentDetails.getTransportMode());
        response.setHouseBill(shipmentDetails.getHouseBill());
        response.setMeasurementDetails(shipmentDetails.getMeasurementDetails());
        response.setCarrierDetails(shipmentDetails.getCarrierDetails());


        return response;
    }

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
            /**
             * Parties Details*
             */
            List<Parties> partiesDetails = createParties(shipmentDetail);
            shipmentDetail.setParties(partiesDetails);
            response.add(shipmentDetail);
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


//    @Transactional
//    public ResponseEntity<?> createSynchronous(CommonRequestModel commonRequestModel) throws Exception {
//        CompleteShipmentRequest request = (CompleteShipmentRequest) commonRequestModel.getData();
//        ShipmentDetails shipmentDetails = jsonHelper.convertValue(request, ShipmentDetails.class);
//        shipmentDetails = shipmentDao.save(shipmentDetails);
//        List<AdditionalDetailRequest> additionalDetailRequest = request.getAdditionalDetailRequest();
//        createAdditionalDetails(shipmentDetails, additionalDetailRequest);
//        List<ContainerRequest> containerRequest = request.getContainerRequest();
//        createContainers(shipmentDetails, containerRequest);
//        return ResponseHelper.buildSuccessResponse(jsonHelper.convertValue(shipmentDetails, ShipmentDetailsResponse.class));
//    }

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
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) throws Exception {
        //ExecutorService executorService = Executors.newFixedThreadPool(100);

        CompleteShipmentRequest request = (CompleteShipmentRequest) commonRequestModel.getData();
        System.out.println(jsonHelper.convertToJson(request));
        ShipmentDetails shipmentDetails = jsonHelper.convertValue(request.getShipmentRequest(), ShipmentDetails.class);

        try {
            getShipment(shipmentDetails);

            List<AdditionalDetailRequest> additionalDetailRequest = request.getAdditionalDetailRequest();
            createAdditionalDetailsAsync(shipmentDetails, additionalDetailRequest);

            List<ContainerRequest> containerRequest = request.getContainerRequest();
            createContainersAsync(shipmentDetails, containerRequest);


            List<PackingRequest> packingRequest = request.getPackingRequest();
            createPackingsAsync(shipmentDetails, packingRequest);


            List<BookingCarriageRequest> bookingCarriageRequest = request.getBookingCarriageRequest();
            createBookingCarriagesAsync(shipmentDetails, bookingCarriageRequest);


            List<ELDetailsRequest> elDetailsRequest = request.getElDetailsRequest();
            createElDetailsAsync(shipmentDetails, elDetailsRequest);


            List<EventsRequest> eventsRequest = request.getEventsRequest();
            createEventsAsync(shipmentDetails, eventsRequest);


            List<FileRepoRequest> fileRepoRequest = request.getFileRepoRequest();
            createFileRepoAsync(shipmentDetails, fileRepoRequest);


            List<JobRequest> jobRequest = request.getJobRequest();
            createJobsAsync(shipmentDetails, jobRequest);


            List<NotesRequest> notesRequest = request.getNotesRequest();
            createNotesAsync(shipmentDetails, notesRequest);


            List<ReferenceNumbersRequest> referenceNumbersRequest = request.getReferenceNumbersRequest();
            createReferenceNumbersAsync(shipmentDetails, referenceNumbersRequest);


            List<RoutingsRequest> routingsRequest = request.getRoutingsRequest();
            createRoutingsAsync(shipmentDetails, routingsRequest);


            List<ServiceDetailsRequest> serviceDetailsRequest = request.getServiceDetailsRequest();
            createServiceDetailsAsync(shipmentDetails, serviceDetailsRequest);


            List<PickupDeliveryDetailsRequest> pickupDeliveryDetailsRequest = request.getPickupDeliveryDetailsRequest();
            createPickupDeliveryAsync(shipmentDetails, pickupDeliveryDetailsRequest);


            List<PartiesRequest> partiesRequest = request.getPartiesRequest();
            createPartiesAsync(shipmentDetails, partiesRequest);


            List<CarrierDetailRequest> carrierDetailRequest = request.getCarrierDetailRequest();
            createCarrierDetailsAsync(shipmentDetails, carrierDetailRequest);

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
        shipmentDao.save(shipmentDetails);
    }

    private void createCarrierDetailsAsync(ShipmentDetails shipmentDetails, List<CarrierDetailRequest> carrierDetailRequest) {
        carrierDetailRequest.forEach(carrierDetail -> {
            createCarrier(shipmentDetails, carrierDetail);
        });
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

    private void createContainersAsync(ShipmentDetails shipmentDetails, List<ContainerRequest> containerRequest) {
        containerRequest.forEach(container -> {
            createContainer(shipmentDetails, container);
        });
    }

    @Transactional
    private void createAdditionalDetailsAsync(ShipmentDetails shipmentDetails, List<AdditionalDetailRequest> additionalDetailRequest) {
        additionalDetailRequest.forEach(additionalDetails -> {
            createAdditionalDetail(shipmentDetails, additionalDetails);
        });
    }

    @Transactional
    public void createbookingCarriage(ShipmentDetails shipmentDetails, BookingCarriageRequest bookingCarriageRequest) {
        bookingCarriageRequest.setShipmentId(shipmentDetails.getId());
        bookingCarriageService.create(CommonRequestModel.buildRequest(bookingCarriageRequest));
    }

    @Transactional
    public void createElDetail(ShipmentDetails shipmentDetails, ELDetailsRequest elDetailsRequest) {
        elDetailsRequest.setShipmentId(shipmentDetails.getId());
        elDetailsService.create(CommonRequestModel.buildRequest(elDetailsRequest));
    }

    @Transactional
    public void createEvent(ShipmentDetails shipmentDetails, EventsRequest eventsRequest) {
        eventsRequest.setShipmentId(shipmentDetails.getId());
        eventService.create(eventsRequest);
    }

    @Transactional
    public void createFileRepo(ShipmentDetails shipmentDetails, FileRepoRequest fileRepoRequest) {
        fileRepoRequest.setEntityId(shipmentDetails.getId());
        fileRepoRequest.setEntityType("SHIPMENT");
        fileRepoService.create(CommonRequestModel.buildRequest(fileRepoRequest));
    }

    @Transactional
    public void createJob(ShipmentDetails shipmentDetails, JobRequest jobRequest) {
        jobRequest.setShipmentId(shipmentDetails.getId());
        jobService.create(CommonRequestModel.buildRequest(jobRequest));
    }

    @Transactional
    public void createNote(ShipmentDetails shipmentDetails, NotesRequest notesRequest) {
        notesRequest.setEntityId(shipmentDetails.getId());
        notesRequest.setEntityType("SHIPMENT");
        notesService.create(CommonRequestModel.buildRequest(notesRequest));
    }

    @Transactional
    public void createParties(ShipmentDetails shipmentDetails, PartiesRequest partiesRequest) {
        partiesRequest.setEntityId(shipmentDetails.getId());
        partiesRequest.setEntityType("SHIPMENT");
        partiesDetailsService.create(CommonRequestModel.buildRequest(partiesRequest));
    }

    @Transactional
    public void createPickupDelivery(ShipmentDetails shipmentDetails, PickupDeliveryDetailsRequest pickupDeliveryDetailsRequest) {
        pickupDeliveryDetailsRequest.setShipmentId(shipmentDetails.getId());
        pickupDeliveryDetailsService.create(CommonRequestModel.buildRequest(pickupDeliveryDetailsRequest));
    }

    @Transactional
    public void createReferenceNumber(ShipmentDetails shipmentDetails, ReferenceNumbersRequest referenceNumbersRequest) {
        referenceNumbersRequest.setShipmentId(shipmentDetails.getId());
        referenceNumbersService.create(CommonRequestModel.buildRequest(referenceNumbersRequest));
    }

    @Transactional
    public void createPacking(ShipmentDetails shipmentDetails, PackingRequest packingRequest) {
        packingRequest.setShipmentId(shipmentDetails.getId());
        packingService.create(CommonRequestModel.buildRequest(packingRequest));
    }

    @Transactional
    public void createContainer(ShipmentDetails shipmentDetails, ContainerRequest containerRequest) {
        containerRequest.setShipmentId(shipmentDetails.getId());
        containerService.create(CommonRequestModel.buildRequest(containerRequest));
    }

    @Transactional
    public void createRouting(ShipmentDetails shipmentDetails, RoutingsRequest routingsRequest) {
        routingsRequest.setShipmentId(shipmentDetails.getId());
        routingsService.create(CommonRequestModel.buildRequest(routingsRequest));
    }

    @Transactional
    public void createServiceDetail(ShipmentDetails shipmentDetails, ServiceDetailsRequest serviceDetailsRequest) {
        serviceDetailsRequest.setShipmentId(shipmentDetails.getId());
        serviceDetailsService.create(CommonRequestModel.buildRequest(serviceDetailsRequest));
    }

    @Transactional
    public void createAdditionalDetail(ShipmentDetails shipmentDetails, AdditionalDetailRequest additionalDetailRequest) {
        additionalDetailRequest.setShipmentId(shipmentDetails.getId());
        additionalDetailService.create(CommonRequestModel.buildRequest(additionalDetailRequest));
    }

    @Transactional
    public void createCarrier(ShipmentDetails shipmentDetails, CarrierDetailRequest carrierDetailRequest) {
        carrierDetailRequest.setShipmentId(shipmentDetails.getId());
        carrierDetailService.create(CommonRequestModel.buildRequest(carrierDetailRequest));
    }

    @Override
    @Transactional
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) throws Exception {
        ShipmentRequest request = (ShipmentRequest) commonRequestModel.getData();
        // TODO- implement Validation logic
        long id = request.getId();
        Optional<ShipmentDetails> oldEntity = shipmentDao.findById(id);
        if (!oldEntity.isPresent()) {
            log.debug("Shipment Details is null for Id {}", request.getId());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        ShipmentDetails entity = jsonHelper.convertValue(request, ShipmentDetails.class);
        entity.setId(oldEntity.get().getId());
        entity = shipmentDao.save(entity);
        return ResponseHelper.buildSuccessResponse(jsonHelper.convertValue(entity, ShipmentDetailsResponse.class));
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
        CompletableFuture<ResponseEntity<?>> packingsFuture = packingService.listAsync(commonListRequestModel);
        CompletableFuture<ResponseEntity<?>> additionalDetailsFuture = additionalDetailService.listAsync(commonListRequestModel);
        CompletableFuture<ResponseEntity<?>> bookingCarriagesFuture = bookingCarriageService.listAsync(commonListRequestModel);
        CompletableFuture<ResponseEntity<?>> containersFuture = containerService.listAsync(commonListRequestModel);
        CompletableFuture<ResponseEntity<?>> elDetailsFuture = elDetailsService.listAsync(commonListRequestModel);
        CompletableFuture<ResponseEntity<?>> eventsFuture = eventService.listAsync(commonListRequestModel);
        CompletableFuture<ResponseEntity<?>> fileRepoFuture = fileRepoService.listAsync(commonListRequestModelbyEntityId);
        CompletableFuture<ResponseEntity<?>> jobsFuture = jobService.listAsync(commonListRequestModel);
        CompletableFuture<ResponseEntity<?>> notesFuture = notesService.listAsync(commonListRequestModelbyEntityId);
        CompletableFuture<ResponseEntity<?>> referenceNumbersFuture = referenceNumbersService.listAsync(commonListRequestModel);
        CompletableFuture<ResponseEntity<?>> routingsFuture = routingsService.listAsync(commonListRequestModel);
        CompletableFuture<ResponseEntity<?>> serviceDetailsFuture = serviceDetailsService.listAsync(commonListRequestModel);
        CompletableFuture<ResponseEntity<?>> carrierDetailsFuture = carrierDetailService.listAsync(commonListRequestModel);
        CompletableFuture<ResponseEntity<?>> pickupDeliveryDetailsFuture =pickupDeliveryDetailsService.listAsync(commonListRequestModel);
        CompletableFuture<ResponseEntity<?>> partiesFuture = partiesDetailsService.listAsync(commonListRequestModelbyEntityId);

        CompletableFuture.allOf(shipmentsFuture, packingsFuture, additionalDetailsFuture, bookingCarriagesFuture, containersFuture,
                elDetailsFuture, eventsFuture, fileRepoFuture, jobsFuture, notesFuture, referenceNumbersFuture, routingsFuture,
                serviceDetailsFuture, pickupDeliveryDetailsFuture, partiesFuture).join();

        var response = new CompleteShipmentResponse();
        var res = (RunnerResponse<ShipmentDetailsResponse>) shipmentsFuture.get().getBody();

        response.setShipment(res.getData());
        response.setPackings(getResponse(packingsFuture));
        response.setAdditionalDetails(getResponse(additionalDetailsFuture));
        response.setBookingCarriages(getResponse(bookingCarriagesFuture));
        response.setContainers(getResponse(containersFuture));
        response.setElDetails(getResponse(elDetailsFuture));
        response.setEvents(getResponse( eventsFuture));
        response.setFileRepo(getResponse(fileRepoFuture));
        response.setJob(getResponse(jobsFuture));
        response.setNotes(getResponse(notesFuture));
        response.setReferenceNumbers(getResponse(referenceNumbersFuture));
        response.setRoutings(getResponse( routingsFuture));
        response.setServiceDetails(getResponse( serviceDetailsFuture));
        response.setPickupDeliveryDetails(getResponse( pickupDeliveryDetailsFuture));
        response.setParties(getResponse(partiesFuture));

        return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private ListCommonRequest constructListCommonRequest(String fieldName, Object value, String operator){
        ListCommonRequest request = new ListCommonRequest();
        request.setPageNo(0);
        request.setLimit(Integer.MAX_VALUE);


        List<FilterCriteria> criterias = new ArrayList<>();
        List<FilterCriteria> innerFilters = new ArrayList();
        Criteria criteria = Criteria.builder().fieldName(fieldName).operator(operator).value(value).build();
        FilterCriteria filterCriteria = FilterCriteria.builder().criteria(criteria).build();
        innerFilters.add(filterCriteria);
        criterias.add(FilterCriteria.builder().innerFilter(innerFilters).logicOperator(criterias.isEmpty() ? null : "or").build());
        request.setFilterCriteria(criterias);
        return request;
    }

    private <T extends IRunnerResponse> List<T> getResponse(CompletableFuture<ResponseEntity<?>> responseEntity) throws ExecutionException, InterruptedException {
        var runnerListResponse = (RunnerListResponse<T>) responseEntity.get().getBody();
        return (List<T>) runnerListResponse.getData();
    }

}
