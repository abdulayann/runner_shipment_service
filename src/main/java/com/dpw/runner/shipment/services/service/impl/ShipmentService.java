package com.dpw.runner.shipment.services.service.impl;


import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.requests.RunnerEntityMapping;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.repository.interfaces.*;
import com.dpw.runner.shipment.services.service.interfaces.*;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
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

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

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
            Map.entry("hblType", RunnerEntityMapping.builder().tableName("blDetails").dataType(String.class).build()),
            Map.entry("transportMode", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("releaseType", RunnerEntityMapping.builder().tableName("blDetails").dataType(String.class).build()),
            Map.entry("deliveryMode", RunnerEntityMapping.builder().tableName("blDetails").dataType(String.class).build()),
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
            Map.entry("screeningStatus", RunnerEntityMapping.builder().tableName("blDetails").dataType(String.class).build()),
            Map.entry("paidPlace", RunnerEntityMapping.builder().tableName("blDetails").dataType(Long.class).build()),
            Map.entry("placeOfIssue", RunnerEntityMapping.builder().tableName("blDetails").dataType(Long.class).build()),
            Map.entry("dateOfIssue", RunnerEntityMapping.builder().tableName("blDetails").dataType(LocalDateTime.class).build()),
            Map.entry("dateOfReceipt", RunnerEntityMapping.builder().tableName("blDetails").dataType(LocalDateTime.class).build()),
            Map.entry("goodsCo", RunnerEntityMapping.builder().tableName("blDetails").dataType(String.class).build()),
            Map.entry("boeDate", RunnerEntityMapping.builder().tableName("blDetails").dataType(LocalDateTime.class).build()),
            Map.entry("boeNumber", RunnerEntityMapping.builder().tableName("blDetails").dataType(String.class).build()),
            Map.entry("shippingLine", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(String.class).build()),
            Map.entry("vessel", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(String.class).build()),
            Map.entry("voyage", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(String.class).build()),
            Map.entry("origin", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(String.class).build()),
            Map.entry("destination", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(String.class).build()),
            Map.entry("eta", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(LocalDateTime.class).build()),
            Map.entry("etd", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(LocalDateTime.class).build()),
            Map.entry("ata", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(LocalDateTime.class).build()),
            Map.entry("weight", RunnerEntityMapping.builder().tableName("measurementDetails").dataType(BigDecimal.class).build()),
            Map.entry("weightUnit", RunnerEntityMapping.builder().tableName("measurementDetails").dataType(String.class).build()),
            Map.entry("volume", RunnerEntityMapping.builder().tableName("measurementDetails").dataType(BigDecimal.class).build()),
            Map.entry("volumeUnit", RunnerEntityMapping.builder().tableName("measurementDetails").dataType(String.class).build()),
            Map.entry("volumetricWeight", RunnerEntityMapping.builder().tableName("measurementDetails").dataType(BigDecimal.class).build()),
            Map.entry("volumetricWeightUnit", RunnerEntityMapping.builder().tableName("measurementDetails").dataType(String.class).build()),
            Map.entry("chargable", RunnerEntityMapping.builder().tableName("measurementDetails").dataType(BigDecimal.class).build()),
            Map.entry("chargeableUnit", RunnerEntityMapping.builder().tableName("measurementDetails").dataType(String.class).build()),
            Map.entry("netWeight", RunnerEntityMapping.builder().tableName("measurementDetails").dataType(BigDecimal.class).build()),
            Map.entry("netWeightUnit", RunnerEntityMapping.builder().tableName("measurementDetails").dataType(String.class).build()),
            Map.entry("noOfPacks", RunnerEntityMapping.builder().tableName("measurementDetails").dataType(Integer.class).build()),
            Map.entry("packsUnit", RunnerEntityMapping.builder().tableName("measurementDetails").dataType(String.class).build()),
            Map.entry("innerPacks", RunnerEntityMapping.builder().tableName("measurementDetails").dataType(Integer.class).build()),
            Map.entry("innerPackUnit", RunnerEntityMapping.builder().tableName("measurementDetails").dataType(String.class).build())
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
             * Bl Details*
             */
            BlDetails blDetail = createBlData();
            shipmentDetail.setBlDetails(blDetail);
            /**
             * Measurement Details*
             */
            MeasurementDetails measurementDetail = createMeasurement();
            shipmentDetail.setMeasurementDetails(measurementDetail);
            /**
             * Carrier Details*
             */
            CarrierDetails carrierDetail = createCarrier();
            shipmentDetail.setCarrierDetails(carrierDetail);
            /**
             * * TODO
             * Pickup Details*
             */
            PickupDetails pickupDetail = createPickup();
            /**
             * * TODO
             * Pickup Details*
             */
            DeliveryDetails deliveryDetail = createDelivery();

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
                    .guid(UUID.randomUUID()).type(partyType).orgId(random).addressId(random)
                    .orgData(ORG).addressData(ADDRESS)
                    .entityId(shipmentDetails.getId()).entityType("SHIPMENT")
                    .build();
            party.setTenantId(1);
            parties.add(party);
        }
        parties = partiesDao.saveAll(parties);
        return parties;
    }

    private DeliveryDetails createDelivery() {
        return null;
    }

    private PickupDetails createPickup() {
        return null;
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

    private MeasurementDetails createMeasurement() {
        int random = new Random().nextInt(100);
        MeasurementDetails measurementDetails = MeasurementDetails.builder().guid(UUID.randomUUID())
                .volume(new BigDecimal(random)).volumeUnit(VOLUME_UNIT.get(new Random().nextInt(100) % VOLUME_UNIT.size()))
                .volumetricWeight(new BigDecimal(random)).volumetricWeightUnit(WEIGHT_UNIT.get(new Random().nextInt(100) % WEIGHT_UNIT.size()))
                .chargable(new BigDecimal(random)).chargeableUnit(VOLUME_UNIT.get(new Random().nextInt(100) % VOLUME_UNIT.size()))
                .netWeight(new BigDecimal(random)).netWeightUnit(WEIGHT_UNIT.get(new Random().nextInt(100) % WEIGHT_UNIT.size()))
                .noOfPacks(random).packsUnit("BAG")
                .build();
        measurementDetails.setTenantId(1);
        return measurementDao.save(measurementDetails);
    }

    private BlDetails createBlData() {
        int random = new Random().nextInt(100);
        BlDetails blDetails = BlDetails.builder()
                .guid(UUID.randomUUID()).releaseType(generateString(3)).hblType(generateString(3))
                .deliveryMode(TRANSPORT_MODES.get(random % TRANSPORT_MODES.size())).screeningStatus(generateString(3))
                .build();
        blDetails.setTenantId(1);
        return blDetailsDao.save(blDetails);
    }

    private ShipmentDetails createShipmentData() {
        int random = new Random().nextInt(100);
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().guid(UUID.randomUUID()).direction(DIRECTIONS.get(random % DIRECTIONS.size())).status(1)
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
        ExecutorService executorService = Executors.newFixedThreadPool(100);

        CompleteShipmentRequest request = (CompleteShipmentRequest) commonRequestModel.getData();
        ShipmentDetails shipmentDetails = jsonHelper.convertValue(request.getShipmentRequest(), ShipmentDetails.class);

        CompletableFuture<Void> createCallToAdditionalDetails;
        CompletableFuture<Void> createCallToContainers;
        CompletableFuture<Void> createCallToPackings;
        CompletableFuture<Void> createCallToBookingCarriages;
        CompletableFuture<Void> createCallToElDetails;
        CompletableFuture<Void> createCallToEvents;
        CompletableFuture<Void> createCallToFileRepos;
        CompletableFuture<Void> createCallToJobs;
        CompletableFuture<Void> createCallToNotes;
        CompletableFuture<Void> createCallToReferenceNumbers;
        CompletableFuture<Void> createCallToRoutings;
        CompletableFuture<Void> createCallToServiceDetails;
        CompletableFuture<Void> createCallToPickupDelivery;
        CompletableFuture<Void> createCallToParties;
        CompletableFuture<Void> createCallToCarrierDetails;

        try {
            getShipment(shipmentDetails);
            List<AdditionalDetailRequest> additionalDetailRequest = request.getAdditionalDetailRequest();
            createCallToAdditionalDetails = CompletableFuture.runAsync(() -> {
                transactionTemplate.execute(status -> {
                    createAdditionalDetailsAsync(shipmentDetails, additionalDetailRequest, executorService);
                    return null;
                });
            });

            List<ContainerRequest> containerRequest = request.getContainerRequest();
            createCallToContainers = CompletableFuture.runAsync(() -> {
                transactionTemplate.execute(status -> {
                    createContainersAsync(shipmentDetails, containerRequest, executorService);
                    return null;
                });
            });

            List<PackingRequest> packingRequest = request.getPackingRequest();
            createCallToPackings = CompletableFuture.runAsync(() -> {
                transactionTemplate.execute(status -> {
                    createPackingsAsync(shipmentDetails, packingRequest, executorService);
                    return null;
                });
            });

            List<BookingCarriageRequest> bookingCarriageRequest = request.getBookingCarriageRequest();
            createCallToBookingCarriages = CompletableFuture.runAsync(() -> {
                transactionTemplate.execute(status -> {
                    createBookingCarriagesAsync(shipmentDetails, bookingCarriageRequest, executorService);
                    return null;
                });
            });

            List<ELDetailsRequest> elDetailsRequest = request.getElDetailsRequest();
            createCallToElDetails = CompletableFuture.runAsync(() -> {
                transactionTemplate.execute(status -> {
                    createElDetailsAsync(shipmentDetails, elDetailsRequest, executorService);
                    return null;
                });
            });

            List<EventsRequest> eventsRequest = request.getEventsRequest();
            createCallToEvents = CompletableFuture.runAsync(() -> {
                transactionTemplate.execute(status -> {
                    createEventsAsync(shipmentDetails, eventsRequest, executorService);
                    return null;
                });
            });

            List<FileRepoRequest> fileRepoRequest = request.getFileRepoRequest();
            createCallToFileRepos = CompletableFuture.runAsync(() -> {
                transactionTemplate.execute(status -> {
                    createFileRepoAsync(shipmentDetails, fileRepoRequest, executorService);
                    return null;
                });
            });

            List<JobRequest> jobRequest = request.getJobRequest();
            createCallToJobs = CompletableFuture.runAsync(() -> {
                transactionTemplate.execute(status -> {
                    createJobsAsync(shipmentDetails, jobRequest, executorService);
                    return null;
                });
            });

            List<NotesRequest> notesRequest = request.getNotesRequest();
            createCallToNotes = CompletableFuture.runAsync(() -> {
                transactionTemplate.execute(status -> {
                    createNotesAsync(shipmentDetails, notesRequest, executorService);
                    return null;
                });
            });

            List<ReferenceNumbersRequest> referenceNumbersRequest = request.getReferenceNumbersRequest();
            createCallToReferenceNumbers = CompletableFuture.runAsync(() -> {
                transactionTemplate.execute(status -> {
                    createReferenceNumbersAsync(shipmentDetails, referenceNumbersRequest, executorService);
                    return null;
                });
            });

            List<RoutingsRequest> routingsRequest = request.getRoutingsRequest();
            createCallToRoutings = CompletableFuture.runAsync(() -> {
                transactionTemplate.execute(status -> {
                    createRoutingsAsync(shipmentDetails, routingsRequest, executorService);
                    return null;
                });
            });

            List<ServiceDetailsRequest> serviceDetailsRequest = request.getServiceDetailsRequest();
            createCallToServiceDetails = CompletableFuture.runAsync(() -> {
                transactionTemplate.execute(status -> {
                    createServiceDetailsAsync(shipmentDetails, serviceDetailsRequest, executorService);
                    return null;
                });
            });

            List<PickupDeliveryDetailsRequest> pickupDeliveryDetailsRequest = request.getPickupDeliveryDetailsRequest();
            createCallToPickupDelivery = CompletableFuture.runAsync(() -> {
                transactionTemplate.execute(status -> {
                    createPickupDeliveryAsync(shipmentDetails, pickupDeliveryDetailsRequest, executorService);
                    return null;
                });
            });

            List<PartiesRequest> partiesRequest = request.getPartiesRequest();
            createCallToParties = CompletableFuture.runAsync(() -> {
                transactionTemplate.execute(status -> {
                    createPartiesAsync(shipmentDetails, partiesRequest, executorService);
                    return null;
                });
            });

            List<CarrierDetailRequest> carrierDetailRequest = request.getCarrierDetailRequest();
            createCallToCarrierDetails = CompletableFuture.runAsync(() -> {
                transactionTemplate.execute(status -> {
                    createCarrierDetailsAsync(shipmentDetails, carrierDetailRequest, executorService);
                    return null;
                });
            });

        } catch (Exception e) {
            log.error(e.getMessage());
            TransactionAspectSupport.currentTransactionStatus().setRollbackOnly();
            throw new RuntimeException(e);
        }

        CompletableFuture.allOf(createCallToAdditionalDetails, createCallToContainers, createCallToPackings, createCallToBookingCarriages, createCallToElDetails, createCallToEvents, createCallToFileRepos, createCallToJobs, createCallToNotes, createCallToReferenceNumbers, createCallToRoutings, createCallToServiceDetails, createCallToPickupDelivery, createCallToParties, createCallToCarrierDetails).join();
        executorService.shutdownNow();
        return ResponseHelper.buildSuccessResponse(jsonHelper.convertValue(shipmentDetails, ShipmentDetailsResponse.class));
    }

    @Transactional
    void getShipment(ShipmentDetails shipmentDetails) {
        shipmentDao.save(shipmentDetails);
    }

    @Transactional
    private CompletableFuture<Void> createCarrierDetailsAsync(ShipmentDetails shipmentDetails, List<CarrierDetailRequest> carrierDetailRequest, ExecutorService executorService) {
        List<CompletableFuture<Void>> futures = carrierDetailRequest.stream()
                .map(request -> CompletableFuture.runAsync(() -> {
                    transactionTemplate.execute(status -> {
                        createCarrier(shipmentDetails, request);
                        log.info("COMPLETED CARRIER DETAIL REQUEST " + request.getId());
                        return null;
                    });
                }, executorService))
                .collect(Collectors.toList());
        var list = CompletableFuture.allOf(futures.toArray(new CompletableFuture[0]));
        list.join();
        return list;
    }

    @Transactional
    private CompletableFuture<Void> createPartiesAsync(ShipmentDetails shipmentDetails, List<PartiesRequest> partiesRequest, ExecutorService executorService) {
        List<CompletableFuture<Void>> futures = partiesRequest.stream()
                .map(request -> CompletableFuture.runAsync(() -> {
                    try {
                        transactionTemplate.execute(status -> {
                            createParties(shipmentDetails, request);
                            log.info("COMPLETED PARTIES REQUEST " + request.getId());
                            return null;
                        });
                    } catch (Exception e) {
                        throw new RuntimeException(e);
                    }
                }, executorService))
                .collect(Collectors.toList());
        var list = CompletableFuture.allOf(futures.toArray(new CompletableFuture[0]));
        list.join();
        return list;
    }

    @Transactional
    private CompletableFuture<Void> createPickupDeliveryAsync(ShipmentDetails shipmentDetails, List<PickupDeliveryDetailsRequest> pickupDeliveryDetailsRequest, ExecutorService executorService) {
        List<CompletableFuture<Void>> futures = pickupDeliveryDetailsRequest.stream()
                .map(request -> CompletableFuture.runAsync(() -> {
                    try {
                        transactionTemplate.execute(status -> {
                            createPickupDelivery(shipmentDetails, request);
                            log.info("COMPLETED PICKUP DELIVERY REQUEST " + request.getId());
                            return null;
                        });
                    } catch (Exception e) {
                        throw new RuntimeException(e);
                    }
                }, executorService))
                .collect(Collectors.toList());
        var list = CompletableFuture.allOf(futures.toArray(new CompletableFuture[0]));
        list.join();
        return list;
    }

    @Transactional
    private CompletableFuture<Void> createServiceDetailsAsync(ShipmentDetails shipmentDetails, List<ServiceDetailsRequest> serviceDetailsRequest, ExecutorService executorService) {
        List<CompletableFuture<Void>> futures = serviceDetailsRequest.stream()
                .map(request -> CompletableFuture.runAsync(() -> {
                    try {
                        transactionTemplate.execute(status -> {
                            createServiceDetail(shipmentDetails, request);
                            log.info("COMPLETED SERVICE DETAIL REQUEST " + request.getId());
                            return null;
                        });
                    } catch (Exception e) {
                        throw new RuntimeException(e);
                    }
                }, executorService))
                .collect(Collectors.toList());
        var list = CompletableFuture.allOf(futures.toArray(new CompletableFuture[0]));
        list.join();
        return list;
    }

    @Transactional
    private CompletableFuture<Void> createRoutingsAsync(ShipmentDetails shipmentDetails, List<RoutingsRequest> routingsRequest, ExecutorService executorService) {
        List<CompletableFuture<Void>> futures = routingsRequest.stream()
                .map(request -> CompletableFuture.runAsync(() -> {
                    try {
                        transactionTemplate.execute(status -> {
                            createRouting(shipmentDetails, request);
                            log.info("COMPLETED ROUTING REQUEST " + request.getId());
                            return null;
                        });
                    } catch (Exception e) {
                        throw new RuntimeException(e);
                    }
                }, executorService))
                .collect(Collectors.toList());
        var list = CompletableFuture.allOf(futures.toArray(new CompletableFuture[0]));
        list.join();
        return list;
    }

    @Transactional
    private CompletableFuture<Void> createReferenceNumbersAsync(ShipmentDetails shipmentDetails, List<ReferenceNumbersRequest> referenceNumbersRequest, ExecutorService executorService) {
        List<CompletableFuture<Void>> futures = referenceNumbersRequest.stream()
                .map(request -> CompletableFuture.runAsync(() -> {
                    try {
                        transactionTemplate.execute(status -> {
                            createReferenceNumber(shipmentDetails, request);
                            log.info("COMPLETED REFERENCE NUMBER REQUEST " + request.getId());
                            return null;
                        });
                    } catch (Exception e) {
                        throw new RuntimeException(e);
                    }
                }, executorService))
                .collect(Collectors.toList());
        var list = CompletableFuture.allOf(futures.toArray(new CompletableFuture[0]));
        list.join();
        return list;
    }

    @Transactional
    private CompletableFuture<Void> createNotesAsync(ShipmentDetails shipmentDetails, List<NotesRequest> notesRequest, ExecutorService executorService) {
        List<CompletableFuture<Void>> futures = notesRequest.stream()
                .map(request -> CompletableFuture.runAsync(() -> {
                    try {
                        transactionTemplate.execute(status -> {
                            createNote(shipmentDetails, request);
                            log.info("COMPLETED NOTE REQUEST " + request.getId());
                            return null;
                        });
                    } catch (Exception e) {
                        throw new RuntimeException(e);
                    }
                }, executorService))
                .collect(Collectors.toList());
        var list = CompletableFuture.allOf(futures.toArray(new CompletableFuture[0]));
        list.join();
        return list;
    }

    @Transactional
    private CompletableFuture<Void> createJobsAsync(ShipmentDetails shipmentDetails, List<JobRequest> jobRequest, ExecutorService executorService) {
        List<CompletableFuture<Void>> futures = jobRequest.stream()
                .map(request -> CompletableFuture.runAsync(() -> {
                    try {
                        transactionTemplate.execute(status -> {
                            createJob(shipmentDetails, request);
                            log.info("COMPLETED JOB REQUEST " + request.getId());
                            return null;
                        });
                    } catch (Exception e) {
                        throw new RuntimeException(e);
                    }
                }, executorService))
                .collect(Collectors.toList());
        var list = CompletableFuture.allOf(futures.toArray(new CompletableFuture[0]));
        list.join();
        return list;
    }

    @Transactional
    private CompletableFuture<Void> createFileRepoAsync(ShipmentDetails shipmentDetails, List<FileRepoRequest> fileRepoRequest, ExecutorService executorService) {
        List<CompletableFuture<Void>> futures = fileRepoRequest.stream()
                .map(request -> CompletableFuture.runAsync(() -> {
                    try {
                        transactionTemplate.execute(status -> {
                            createFileRepo(shipmentDetails, request);
                            log.info("COMPLETED FILE REPO REQUEST " + request.getId());
                            return null;
                        });
                    } catch (Exception e) {
                        throw new RuntimeException(e);
                    }
                }, executorService))
                .collect(Collectors.toList());
        var list = CompletableFuture.allOf(futures.toArray(new CompletableFuture[0]));
        list.join();
        return list;
    }

    @Transactional
    private CompletableFuture<Void> createEventsAsync(ShipmentDetails shipmentDetails, List<EventsRequest> eventsRequest, ExecutorService executorService) {
        List<CompletableFuture<Void>> futures = eventsRequest.stream()
                .map(request -> CompletableFuture.runAsync(() -> {
                    try {
                        transactionTemplate.execute(status -> {
                            createEvent(shipmentDetails, request);
                            log.info("COMPLETED EVENT REQUEST " + request.getId());
                            return null;
                        });
                    } catch (Exception e) {
                        throw new RuntimeException(e);
                    }
                }, executorService))
                .collect(Collectors.toList());
        var list = CompletableFuture.allOf(futures.toArray(new CompletableFuture[0]));
        list.join();
        return list;
    }

    @Transactional
    private CompletableFuture<Void> createElDetailsAsync(ShipmentDetails shipmentDetails, List<ELDetailsRequest> elDetailsRequest, ExecutorService executorService) {
        List<CompletableFuture<Void>> futures = elDetailsRequest.stream()
                .map(request -> CompletableFuture.runAsync(() -> {
                    try {
                        transactionTemplate.execute(status -> {
                            createElDetail(shipmentDetails, request);
                            log.info("COMPLETED EL DETAIL REQUEST " + request.getId());
                            return null;
                        });
                    } catch (Exception e) {
                        throw new RuntimeException(e);
                    }
                }, executorService))
                .collect(Collectors.toList());
        var list = CompletableFuture.allOf(futures.toArray(new CompletableFuture[0]));
        list.join();
        return list;
    }

    @Transactional
    private CompletableFuture<Void> createBookingCarriagesAsync(ShipmentDetails shipmentDetails, List<BookingCarriageRequest> bookingCarriageRequest, ExecutorService executorService) {
        List<CompletableFuture<Void>> futures = bookingCarriageRequest.stream()
                .map(request -> CompletableFuture.runAsync(() -> {
                    try {
                        transactionTemplate.execute(status -> {
                            createbookingCarriage(shipmentDetails, request);
                            log.info("COMPLETED BOOKING CARRIAGE REQUEST " + request.getId());
                            return null;
                        });
                    } catch (Exception e) {
                        throw new RuntimeException(e);
                    }
                }, executorService))
                .collect(Collectors.toList());
        var list = CompletableFuture.allOf(futures.toArray(new CompletableFuture[0]));
        list.join();
        return list;
    }

    @Transactional
    private CompletableFuture<Void> createPackingsAsync(ShipmentDetails shipmentDetails, List<PackingRequest> packingRequest, ExecutorService executorService) {
        List<CompletableFuture<Void>> futures = packingRequest.stream()
                .map(request -> CompletableFuture.runAsync(() -> {
                    try {
                        transactionTemplate.execute(status -> {
                            createPacking(shipmentDetails, request);
                            log.info("COMPLETED PACKING REQUEST " + request.getId());
                            return null;
                        });
                    } catch (Exception e) {
                        throw new RuntimeException(e);
                    }
                }, executorService))
                .collect(Collectors.toList());
        var list = CompletableFuture.allOf(futures.toArray(new CompletableFuture[0]));
        list.join();
        return list;
    }

    @Transactional
    private CompletableFuture<Void> createContainersAsync(ShipmentDetails shipmentDetails, List<ContainerRequest> containerRequest, ExecutorService executorService) {
        List<CompletableFuture<Void>> futures = containerRequest.stream()
                .map(request -> CompletableFuture.runAsync(() -> {
                    try {
                        transactionTemplate.execute(status -> {
                            createContainer(shipmentDetails, request);
                            log.info("COMPLETED CONTAINERS CREATE REQUEST " + request.getId());
                            return null;
                        });
                    } catch (Exception e) {
                        throw new RuntimeException(e);
                    }
                }, executorService))
                .collect(Collectors.toList());
        var list = CompletableFuture.allOf(futures.toArray(new CompletableFuture[0]));
        list.join();
        return list;
    }

    @Transactional
    private CompletableFuture<Void> createAdditionalDetailsAsync(ShipmentDetails shipmentDetails, List<AdditionalDetailRequest> additionalDetailRequest, ExecutorService executorService) {
        List<CompletableFuture<Void>> futures = additionalDetailRequest.stream()
                .map(request -> CompletableFuture.runAsync(() -> {
                    try {
                        transactionTemplate.execute(status -> {
                            createAdditionalDetail(shipmentDetails, request);
                            log.info("COMPLETED ADDITIONAL DETAIL CREATE REQUEST " + request.getId());
                            return null;
                        });
                    } catch (Exception e) {
                        throw new RuntimeException(e);
                    }
                }, executorService))
                .collect(Collectors.toList());
        var list = CompletableFuture.allOf(futures.toArray(new CompletableFuture[0]));
        list.join();
        return list;
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

}
