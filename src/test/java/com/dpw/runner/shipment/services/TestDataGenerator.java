//package com.dpw.runner.shipment.services;
//
//import com.dpw.runner.shipment.services.dao.interfaces.IPartiesDao;
//import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
//import com.dpw.runner.shipment.services.dto.request.*;
//import com.dpw.runner.shipment.services.entity.BookingCarriage;
//import com.dpw.runner.shipment.services.entity.ShipmentDetails;
//import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
//import com.dpw.runner.shipment.services.mapper.BookingCarriageMapper;
//import com.dpw.runner.shipment.services.mapper.ShipmentDetailsMapper;
//import org.mapstruct.factory.Mappers;
//import org.springframework.beans.factory.annotation.Autowired;
//import org.springframework.stereotype.Component;
//
//import java.math.BigDecimal;
//import java.time.LocalDateTime;
//import java.util.*;
//
//@Component
//public class TestDataGenerator {
//    public IShipmentDao shipmentDao;
//    private IPartiesDao partiesDao;
//    private BookingCarriageMapper bookingCarriageMapper;
//    private ShipmentDetailsMapper shipmentDetailsMapper;
//
//    private List<String> TRANSPORT_MODES = Arrays.asList("SEA", "ROAD", "RAIL", "AIR");
//    private List<String> SHIPMENT_TYPE = Arrays.asList("FCL", "LCL");
//    private List<String> WEIGHT_UNIT = Arrays.asList("KGS", "G", "DT");
//    private List<String> VOLUME_UNIT = Arrays.asList("M3", "L3", "CC");
//    private List<String> SHIPPING_LINE = Arrays.asList("DPWC", "MARUSK", "APLU");
//    private List<String> LOCATIONS = Arrays.asList("Jabel Ali", "Nava Shiva", "Shanghai", "Vancouver", "Seattle");
//    private List<String> PARTY_TYPE = Arrays.asList("CLIENT", "CONSIGNER", "CONSIGNEE");
//    private List<String> DIRECTIONS = Arrays.asList("IMP", "EXP");
//    private List<String> SOURCE = Arrays.asList("API", "Runner", "Logistics");
//    private List<String> CONTAINER_CODES = Arrays.asList("20FR", "40FR", "20GP", "40GP");
//
//    private List<String> CARRIAGE_TYPES = Arrays.asList("Main", "OnCarriage", "PreCarriage");
//
//    private List<String> PACKS_TYPE = Arrays.asList("BAG", "AMM", "BAL", "BBG", "BBK", "CNT");
//
//    private List<String> REFERENCE_NUMBER_TYPES = Arrays.asList("AMS", "BKG", "BLE", "CAR", "COC", "COM");
//    private List<String> PD_TYPE = Arrays.asList("PICKUP", "DELIVERY");
//
//    private List<String> SERVICE_TYPES = Arrays.asList("CHO", "CHN", "DRYAGE", "FCS", "FUN");
//
//
//    private Map<String, Object> ADDRESS = Map.ofEntries(
//            Map.entry("AddressShortCode", "Default")
//    );
//    private Map<String, Object> ORG = Map.ofEntries(
//            Map.entry("TenantName", "DP WORLD LOGISTICS CANADA INC")
//    );
//
//    @Autowired
//    public TestDataGenerator(IShipmentDao shipmentDao,
//                             IPartiesDao partiesDao, BookingCarriageMapper bookingCarriageMapper, ShipmentDetailsMapper shipmentDetailsMapper) {
//        this.shipmentDao = shipmentDao;
//        this.partiesDao = partiesDao;
//        this.bookingCarriageMapper = bookingCarriageMapper;
//        this.shipmentDetailsMapper = shipmentDetailsMapper;
//    }
//
//    public List<ShipmentDetails> populateH2WithTestData() throws RunnerException {
//        List<ShipmentDetails> response = new ArrayList<>();
//        ShipmentDetails shipmentDetail1 = ShipmentDetails.builder()
////                .id(1L)
//                .status(1)
//                .direction("EXP")
//                .source("API")
//                .transportMode("SEA")
//                .shipmentType("FCL")
//                .houseBill("FTOGI1283602230TzrNGM")
//                .masterBill("9g4e5ayd93")
//                .bookingReference("0F3RY53NJH")
//                .consolRef("13073QR1N5")
//                .paymentTerms("C1E")
//                .goodsDescription("8WM13OLH9R")
//                .additionalTerms("ACJ6O7ZVX2")
//                .build();
//        shipmentDetail1.setTenantId(1);
//        shipmentDao.save(shipmentDetail1, false);
//        response.add(shipmentDetail1);
//
//        //**************END
//
//        ShipmentDetails shipmentDetail2 = ShipmentDetails.builder()
////                .id(2L)
//                .status(0)
//                .direction("EXP")
//                .source("API")
//                .transportMode("AIR")
//                .shipmentType("LCL")
//                .houseBill("FT0TzrNGM")
//                .masterBill("12344AYD93")
//                .bookingReference("0F3RY5333H")
//                .consolRef("13073QR1N5")
//                .paymentTerms("C1E")
//                .goodsDescription("8W2323OLH9R")
//                .additionalTerms("ABCD6O7ZVX2")
//                .build();
//        shipmentDetail2.setTenantId(1);
//
//        shipmentDao.save(shipmentDetail2, false);
//        response.add(shipmentDetail2);
//
//        //*************END
//
//        ShipmentDetails shipmentDetail3 = ShipmentDetails.builder()
////                .id(3L)
//                .status(0)
//                .direction("EXP")
//                .source("API")
//                .transportMode("AIR")
//                .shipmentType("LCL")
//                .houseBill("FTOGI1283602230TzrNGM")
//                .masterBill("12344AYD94")
//                .bookingReference("0F3RY5334H")
//                .consolRef("13073QR1N6")
//                .paymentTerms("C4E")
//                .goodsDescription("8W2323OLH0R")
//                .additionalTerms("ABCD7O7ZVX2")
//                .build();
//        shipmentDetail3.setTenantId(1);
//
//        shipmentDao.save(shipmentDetail3, false);
//        response.add(shipmentDetail3);
//
//        //*************END
//
//        ShipmentDetails shipmentDetail4 = ShipmentDetails.builder()
////                .id(4L)
//                .status(0)
//                .direction("EXP")
//                .source("Runner")
//                .transportMode("AIR")
//                .shipmentType("LCL")
//                .houseBill("FT0TzrNG4")
//                .masterBill("12344AYD95")
//                .bookingReference("0F3RY5335H")
//                .consolRef("13073QR1N9")
//                .paymentTerms("C5E")
//                .goodsDescription("0W2323OLH1R")
//                .additionalTerms("ABCD7O7ZVX5")
//                .build();
//        shipmentDetail4.setTenantId(1);
//
//        shipmentDao.save(shipmentDetail4, false);
//        response.add(shipmentDetail4);
//
//        //****END
//
//        ShipmentDetails shipmentDetail5 = ShipmentDetails.builder()
////                .id(5L)
//                .shipmentId("SHP000102015")
//                .status(0)
//                .direction("EXP")
//                .source("API")
//                .transportMode("SEA")
//                .shipmentType("FCL")
//                .houseBill("FTOGI128360230TzrNGM")
//                .masterBill("9G4E5AYD923")
//                .bookingReference("0F3RY53NJH")
//                .consolRef("13073QR1N5")
//                .paymentTerms("C2E")
//                .goodsDescription("8WM13OLH0R")
//                .additionalTerms("ACJ6O7ZVX4")
//                .build();
//        shipmentDetail5.setTenantId(1);
//        shipmentDao.save(shipmentDetail5, false);
//        response.add(shipmentDetail5);
//
//        return response;
//    }
//
//    public List<ShipmentRequest> createTestShipment(int count) {
//
//        List<ShipmentRequest> requests = new ArrayList<>();
//        /**
//         * BL details
//         * Measurements
//         * carrier
//         * pickup
//         * Delivery* *
//         * Shipment details
//         * Parties*
//         * * * * * * *
//         * * * */
//
//        for (int i = 0; i < count; i++) {
//            ShipmentRequest shipmentRequest1 = createShipmentData();
//
//            shipmentRequest1.setCarrierDetails(createCarrier());
//
//            /**
//             * Parties Details*
//             */
//            List<PartiesRequest> partiesRequestsList = createParties();
//
//            PartiesRequest client = partiesRequestsList.stream().filter(o -> o.getType() == "CLIENT").findFirst().get();
//            PartiesRequest consigner = partiesRequestsList.stream().filter(o -> o.getType() == "CLIENT").findFirst().get();
//            PartiesRequest consignee = partiesRequestsList.stream().filter(o -> o.getType() == "CLIENT").findFirst().get();
//
//            shipmentRequest1.setClient(client);
//            shipmentRequest1.setConsignee(consignee);
//            shipmentRequest1.setConsigner(consigner);
//
//            shipmentRequest1.setPackingList(createPackingRequest(new Random().nextInt(10)));
//            shipmentRequest1.setAdditionalDetails(createAdditionalData());
//            shipmentRequest1.setBookingCarriagesList(createBookingCarriageRequest(new Random().nextInt(10)));
//            shipmentRequest1.setContainersList(createContainerRequest(new Random().nextInt(10)));
//            shipmentRequest1.setElDetailsList(createElDetailsRequest(new Random().nextInt(10)));
//            shipmentRequest1.setEventsList(createEventsRequest(new Random().nextInt(10)));
//            shipmentRequest1.setFileRepoList(createFileRepoRequest(new Random().nextInt(10)));
//            shipmentRequest1.setJobsList(createJobRequest(new Random().nextInt(10)));
//            shipmentRequest1.setNotesList(createNotesRequest(new Random().nextInt(10)));
//            shipmentRequest1.setReferenceNumbersList(createReferenceNumberRequest(new Random().nextInt(10)));
//            shipmentRequest1.setRoutingsList(createRountingsRequest(new Random().nextInt(10)));
//            shipmentRequest1.setServicesList(createServiceDetailsRequest(10));
//
//            requests.add(shipmentRequest1);
//        }
//
//        return requests;
//    }
//
//    private List<ServiceDetailsRequest> createServiceDetailsRequest(int count) {
//        List<ServiceDetailsRequest> list = new ArrayList<>();
//        for (int i = 0; i < count; i++) {
//            int random = new Random().nextInt(10);
//            list.add(ServiceDetailsRequest.builder()
//                    .serviceType(SERVICE_TYPES.get(new Random().nextInt(100) % SERVICE_TYPES.size()))
////                    .srvLocation(random)
//                    .bookingDate(LocalDateTime.now())
//                    .serviceCount((long) (random))
//                    .completionDate(LocalDateTime.now())
//                    .refNumber(generateString(7))
//                    .serviceNotes(generateString(15))
//                    .build());
//        }
//        return list;
//    }
//
//    private List<PickupDeliveryDetailsRequest> createPickupDeliverDetailsRequest(int count) {
//        List<PickupDeliveryDetailsRequest> list = new ArrayList<>();
//        for (String partyType : PD_TYPE) {
//            int random = new Random().nextInt(10);
//            list.add(PickupDeliveryDetailsRequest.builder()
//                    .type(partyType)
//                    .estimatedPickupOrDelivery(LocalDateTime.now())
//                    .actualPickupOrDelivery(LocalDateTime.now())
//                    .pickupOrDelivery(LocalDateTime.now())
//                    .requiredBy(LocalDateTime.now())
//                    .portTransportAdvised(LocalDateTime.now())
//                    .build());
//
//        }
//        return list;
//    }
//
//    private List<RoutingsRequest> createRountingsRequest(int count) {
//        List<RoutingsRequest> list = new ArrayList<>();
//        for (int i = 0; i < count; i++) {
//            int random = new Random().nextInt(10);
//            list.add(RoutingsRequest.builder()
//                    .leg((long) random)
//                    .mode(TRANSPORT_MODES.get(new Random().nextInt(100) % TRANSPORT_MODES.size()))
//                    .vesselName(generateString(5)).voyage(generateString(5))
//                    .eta(LocalDateTime.now())
//                    .etd(LocalDateTime.now())
//                    .ata(LocalDateTime.now())
//                    .atd(LocalDateTime.now())
//                    .pol(generateString(5))
//                    .pod(generateString(5))
//                    .build());
//        }
//        return list;
//    }
//
//    private List<ReferenceNumbersRequest> createReferenceNumberRequest(int count) {
//        List<ReferenceNumbersRequest> referenceNumbersRequests = new ArrayList<>();
//        for (int i = 0; i < count; i++) {
//            int random = new Random().nextInt(10);
//            referenceNumbersRequests.add(ReferenceNumbersRequest.builder()
//                    .referenceNumber(generateString(6))
//                    .type(REFERENCE_NUMBER_TYPES.get(new Random().nextInt(100) % REFERENCE_NUMBER_TYPES.size()))
//                    .countryOfIssue("IND")
//                    .build());
//        }
//        return referenceNumbersRequests;
//    }
//
//    private List<NotesRequest> createNotesRequest(int count) {
//        List<NotesRequest> notesRequests = new ArrayList<>();
//        for (int i = 0; i < count; i++) {
//            int random = new Random().nextInt(10);
//            notesRequests.add(NotesRequest.builder()
//                    .text(generateString(15))
//                    .insertDate(LocalDateTime.now())
//                    .insertUserId("HIPL")
//                    .insertUserDisplayName("HIPL")
//                    .build());
//        }
//        return notesRequests;
//    }
//
//    private List<JobRequest> createJobRequest(int count) {
//        List<JobRequest> jobRequests = new ArrayList<>();
//        for (int i = 0; i < count; i++) {
//            int random = new Random().nextInt(10);
//            jobRequests.add(JobRequest.builder()
//                    .orderDate(LocalDateTime.now())
//                    .orderNumber(generateString(10))
//                    .confirmDate(LocalDateTime.now())
//                    .confirmNumber(generateString(10))
//                    .invoiceDate(LocalDateTime.now())
//                    .invoiceNumber(generateString(10))
//                    .additionalTerms(generateString(10))
//                    .build());
//        }
//        return jobRequests;
//    }
//
//    private List<FileRepoRequest> createFileRepoRequest(int count) {
//        List<FileRepoRequest> fileRepoRequests = new ArrayList<>();
//        for (int i = 0; i < count; i++) {
//            int random = new Random().nextInt(10);
//            fileRepoRequests.add(FileRepoRequest.builder()
//                    .fileName(generateString(5))
//                    .path(generateString(7))
//                    .docType("HBL")
//                    .clientEnabled(random % 2 == 0)
//                    .isPosted(random % 2 == 0)
//                    .build());
//        }
//        return fileRepoRequests;
//    }
//
//    private List<EventsRequest> createEventsRequest(int count) {
//        List<EventsRequest> eventsRequests = new ArrayList<>();
//        for (int i = 0; i < count; i++) {
//            int random = new Random().nextInt(10);
//            eventsRequests.add(EventsRequest.builder()
//                    .description(generateString(10))
//                    .estimated(LocalDateTime.now())
//                    .actual(LocalDateTime.now())
//                    .isPublicTrackingEvent(random % 2 == 0)
//                    .placeName(generateString(10))
//                    .placeDescription(generateString(10))
//                    .source("SHIPMENT")
//                    .build());
//        }
//        return eventsRequests;
//    }
//
//
//    /**
//     * TODO : CORRECTION IN REQUEST MODELS
//     * <p>
//     * Weight -> BigDecimal in ELDetails
//     * Change all Date to LocalDateTime
//     * Check ServiceDetailsRequest -> ServiceCount
//     *
//     * @param count
//     * @return
//     */
//    private List<ELDetailsRequest> createElDetailsRequest(int count) {
//        List<ELDetailsRequest> elDetailsRequests = new ArrayList<>();
//        for (int i = 0; i < count; i++) {
//            int random = new Random().nextInt(10);
//            elDetailsRequests.add(
//                    ELDetailsRequest.builder()
//                            .elNumber(String.valueOf(random))
//                            .packages((long) random)
//                            .weight((long) (random))
//                            .weightUnit(WEIGHT_UNIT.get(new Random().nextInt(100) % WEIGHT_UNIT.size()))
//                            .build()
//            );
//        }
//        return elDetailsRequests;
//    }
//
//    private List<ContainerRequest> createContainerRequest(int count) {
//        List<ContainerRequest> containerRequests = new ArrayList<>();
//        for (int i = 0; i < count; i++) {
//            int random = new Random().nextInt(10);
//            containerRequests.add(ContainerRequest.builder()
//                    .containerCode(CONTAINER_CODES.get(new Random().nextInt(10) % CONTAINER_CODES.size()))
//                    .containerCount((long) random)
//                    .containerNumber("CONT0000" + random)
//                    .grossWeight(BigDecimal.valueOf(random))
//                    .grossWeightUnit(WEIGHT_UNIT.get(new Random().nextInt(100) % WEIGHT_UNIT.size()))
//                    .chargeable(BigDecimal.valueOf(random))
//                    .grossVolume(BigDecimal.valueOf(new Random().nextInt(100)))
//                    .grossVolumeUnit(VOLUME_UNIT.get(new Random().nextInt(100) % VOLUME_UNIT.size())).build());
//        }
//        return containerRequests;
//    }
//
//    private List<PackingRequest> createPackingRequest(int count) {
//
//        List<PackingRequest> packingRequests = new ArrayList<>();
//        for (int i = 0; i < count; i++) {
//            int random = new Random().nextInt(10);
//
//            packingRequests.add(PackingRequest.builder().packs(random + "")
//                    .packsType(PACKS_TYPE.get(new Random().nextInt(100) % PACKS_TYPE.size()))
//                    .weight(BigDecimal.valueOf(random))
//                    .weightUnit(WEIGHT_UNIT.get(new Random().nextInt(100) % WEIGHT_UNIT.size()))
//                    .chargeable(BigDecimal.valueOf(random))
//                    .commodity(generateString(10))
//                    .volume(BigDecimal.valueOf(new Random().nextInt(100)))
//                    .volumeUnit(VOLUME_UNIT.get(new Random().nextInt(100) % VOLUME_UNIT.size())).build());
//
//        }
//        return packingRequests;
//
//    }
//
//    private List<BookingCarriageRequest> createBookingCarriageRequest(int count) {
//        BookingCarriageMapper mapper = Mappers.getMapper(BookingCarriageMapper.class);
//
//        List<BookingCarriageRequest> bookingCarriageRequests = new ArrayList<>();
//        for (int i = 0; i < count; i++) {
//            int random = new Random().nextInt(10);
//            bookingCarriageRequests.add(mapper.getRequest(BookingCarriage.builder()
//                    .eta(LocalDateTime.now()).etd(LocalDateTime.now())
//                    .vessel(generateString(5)).voyage(generateString(5))
//                    .carriageType(CARRIAGE_TYPES.get(new Random().nextInt(10) % CARRIAGE_TYPES.size()))
//                    .build()));
//        }
//        return bookingCarriageRequests;
//    }
//
//    private AdditionalDetailRequest createAdditionalData() {
//        int random = new Random().nextInt(100);
//
//        return AdditionalDetailRequest.builder().releaseType(generateString(3)).houseBillType(generateString(3))
//                .deliveryMode(TRANSPORT_MODES.get(random % TRANSPORT_MODES.size())).screeningStatus(Arrays.asList(generateString(3))).original(1).printedOriginal(true)
//                .hsnNumber(Integer.toUnsignedLong(random))
//                .BOENumber(generateString(10)).build();
//
//    }
//
////    private List<IRunnerResponse> convertEntityListToDtoList(List<ShipmentDetails> lst) {
////        List<IRunnerResponse> responseList = new ArrayList<>();
////        lst.forEach(shipmentDetail -> {
////            responseList.add(jsonHelper.convertValue(shipmentDetail, ShipmentDetailsResponse.class));
////        });
////        return responseList;
////    }
//
//    private List<PartiesRequest> createParties() {
//        List<PartiesRequest> parties = new ArrayList<>();
//        int random = new Random().nextInt(100);
//        for (String partyType : PARTY_TYPE) {
//            PartiesRequest party = PartiesRequest.builder()
//                    .type(partyType)
//                    .orgCode(generateString(10))
//                    .addressCode(generateString(10))
//                    .orgData(ORG)
//                    .addressData(ADDRESS)
//                    .build();
//            party.setTenantId(1);
//            parties.add(party);
//        }
//        return parties;
//    }
//
//
//    private CarrierDetailRequest createCarrier() {
//        int random = new Random().nextInt(100);
//        CarrierDetailRequest carrier = CarrierDetailRequest.builder()
//                .shippingLine(SHIPPING_LINE.get(random % SHIPPING_LINE.size()))
//                .vessel(generateString(5)).voyage(generateString(5)).origin(LOCATIONS.get(random % LOCATIONS.size())).destination(LOCATIONS.get(random % LOCATIONS.size()))
//                .eta(LocalDateTime.now()).etd(LocalDateTime.now()).ata(LocalDateTime.now()).atd(LocalDateTime.now())
//                .build();
//        return carrier;
//    }
//
//    private ShipmentRequest createShipmentData() {
//        int random = new Random().nextInt(100);
//        return shipmentDetailsMapper.getRequest(ShipmentDetails.builder()
//                .direction(DIRECTIONS.get(random % DIRECTIONS.size())).status(random % 2)
//                .source(SOURCE.get(random % SOURCE.size())).transportMode(TRANSPORT_MODES.get(random % TRANSPORT_MODES.size())).shipmentType(SHIPMENT_TYPE.get(random % SHIPMENT_TYPE.size()))
//                .houseBill(generateString(10)).masterBill(generateString(10)).bookingReference(generateString(10)).consolRef(generateString(10)).paymentTerms(generateString(3))
//                .goodsDescription(generateString(10)).additionalTerms(generateString(10)).volume(new BigDecimal(random)).volumeUnit(VOLUME_UNIT.get(new Random().nextInt(100) % VOLUME_UNIT.size()))
//                .volumetricWeight(new BigDecimal(random)).volumetricWeightUnit(WEIGHT_UNIT.get(new Random().nextInt(100) % WEIGHT_UNIT.size()))
//                .chargable(new BigDecimal(random)).chargeableUnit(VOLUME_UNIT.get(new Random().nextInt(100) % VOLUME_UNIT.size()))
//                .netWeight(new BigDecimal(random)).netWeightUnit(WEIGHT_UNIT.get(new Random().nextInt(100) % WEIGHT_UNIT.size()))
//                .noOfPacks(random).packsUnit("BAG").shipmentId("SHP000" + new Random().nextInt(40))
//                .build());
//    }
//
//
////    @Transactional
////    public ResponseEntity<?> createSynchronous(CommonRequestModel commonRequestModel) throws RunnerException {
////        CompleteShipmentRequest request = (CompleteShipmentRequest) commonRequestModel.getData();
////        ShipmentDetails shipmentDetails = jsonHelper.convertValue(request, ShipmentDetails.class);
////        shipmentDetails = shipmentDao.save(shipmentDetails);
////        List<AdditionalDetailRequest> additionalDetailRequest = request.getAdditionalDetailRequest();
////        createAdditionalDetails(shipmentDetails, additionalDetailRequest);
////        List<ContainerRequest> containerRequest = request.getContainerRequest();
////        createContainers(shipmentDetails, containerRequest);
////        return ResponseHelper.buildSuccessResponse(jsonHelper.convertValue(shipmentDetails, ShipmentDetailsResponse.class));
////    }
//
//    private String generateString(int length) {
//        String SALTCHARS = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890";
//        StringBuilder salt = new StringBuilder();
//        Random rnd = new Random();
//        while (salt.length() < length) {
//            int index = (int) (rnd.nextFloat() * SALTCHARS.length());
//            salt.append(SALTCHARS.charAt(index));
//        }
//        return salt.toString();
//    }
//
//}
