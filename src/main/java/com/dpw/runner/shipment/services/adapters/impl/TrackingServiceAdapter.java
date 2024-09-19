package com.dpw.runner.shipment.services.adapters.impl;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.IsStringNullOrEmpty;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;

import com.azure.messaging.servicebus.ServiceBusMessage;
import com.dpw.runner.shipment.services.adapters.config.TrackingServiceConfig;
import com.dpw.runner.shipment.services.adapters.interfaces.ITrackingServiceAdapter;
import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.commons.constants.EventConstants;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IAwbDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.CarrierListObject;
import com.dpw.runner.shipment.services.dto.request.TrackingRequest;
import com.dpw.runner.shipment.services.dto.response.TrackingEventsResponse;
import com.dpw.runner.shipment.services.dto.trackingservice.TrackingServiceApiRequest;
import com.dpw.runner.shipment.services.dto.trackingservice.TrackingServiceApiResponse;
import com.dpw.runner.shipment.services.dto.trackingservice.UniversalTrackingPayload;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.entity.Awb;
import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.entity.ConsoleShipmentMapping;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.dto.CarrierMasterData;
import com.dpw.runner.shipment.services.masterdata.factory.MasterDataFactory;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service_bus.ISBProperties;
import com.dpw.runner.shipment.services.service_bus.ISBUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.nimbusds.jose.util.Pair;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.ForkJoinPool;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;


@Slf4j
@Service
public class TrackingServiceAdapter implements ITrackingServiceAdapter {

    @Autowired
    private IAwbDao awbDao;
    @Autowired
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;
    @Autowired
    private IConsolidationDetailsDao consolidationDetailsDao;
    @Autowired
    private ISBProperties isbProperties;
    @Autowired
    private JsonHelper jsonHelper;
    @Autowired
    private MasterDataFactory masterDataFactory;
    @Autowired
    private RestTemplate restTemplate;
    @Autowired
    private ISBUtils sbUtils;
    @Autowired
    private IShipmentDao shipmentDao;
    @Value("${trackingService.apiKey}")
    private String trackingServiceApiKey;
    @Autowired
    private TrackingServiceConfig trackingServiceConfig;
    @Value("${trackingService.newFlowEndpoint.url}")
    private String trackingServiceNewFlowEndpoint;
    @Autowired
    private IV1Service v1Service;

    @Override
    public UniversalTrackingPayload.UniversalEventsPayload mapEventDetailsForTracking(String bookingReferenceNumber, String referenceNumberType, String runnerReferenceNumber, List<Events> events) {
        return UniversalTrackingPayload.UniversalEventsPayload.builder()
            .runnerReferenceNumber(runnerReferenceNumber)
            .referenceNumberType(referenceNumberType)
            .bookingReferenceNumber(bookingReferenceNumber)
            .events(mapEvents(events))
            .build();
    }


    public List<UniversalTrackingPayload.EventDetail> mapEvents(List<Events> events)
    {
        List<UniversalTrackingPayload.EventDetail> infoList = new ArrayList<>();
        events.forEach(i -> {
            UniversalTrackingPayload.EventDetail eventDetail = UniversalTrackingPayload.EventDetail.builder()
                    .id(i.getId())
                    .description(i.getDescription())
                    .eventCode(i.getEventCode())
                    .estimated(i.getEstimated())
                    .actual(i.getActual())
                    .placeName(i.getPlaceName())
                    .build();
            infoList.add(eventDetail);
        });
        return infoList;
    }

    @Override
    @Async
    public void publishUpdatesToTrackingServiceQueue(String jsonBody, Boolean isTrackingEvents) {
        if(isTrackingEvents){
          // Publish to EventsMessage Topic
          sbUtils.sendMessagesToTopic(
              isbProperties,
              trackingServiceConfig.getEventsMessageTopic(),
              List.of(new ServiceBusMessage(jsonBody)));
        } else {
            sbUtils.sendMessagesToTopic(
                    isbProperties,
                    trackingServiceConfig.getRunnerFlowMessageTopic(),
                    List.of(new ServiceBusMessage(jsonBody)));
        }
    }

    @Override
    public boolean checkIfConsolAttached(ShipmentDetails shipmentDetails) {
        boolean res = false;
        try {
            List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByShipmentId(shipmentDetails.getId());
            ConsolidationDetails consolidationDetails = null;
            if(consoleShipmentMappings != null && consoleShipmentMappings.size() > 0) {
                Optional<ConsolidationDetails> optional = consolidationDetailsDao.findById(consoleShipmentMappings.get(0).getConsolidationId());
                consolidationDetails = optional.get();
            }

            if(consolidationDetails != null && !consolidationDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR))
                res = GetContainerDetailsAttachedForShipment(shipmentDetails).size() > 0;
            else
                res = !Objects.isNull(shipmentDetails.getHouseBill()) || !Objects.isNull(shipmentDetails.getMasterBill());

        } catch (Exception e) {
            log.error(e.getMessage());
        }
        return res;
    }

    @Override
    public boolean checkIfAwbExists(ConsolidationDetails consolidationDetails) {
        List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByConsolidationId(consolidationDetails.getId());
        if(consoleShipmentMappings != null && consoleShipmentMappings.size() > 0){
            Optional<ShipmentDetails> optional = shipmentDao.findById(consoleShipmentMappings.get(0).getShipmentId());
            var shipment = optional.get();
            return (shipment.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) && shipment.getHouseBill() != null);
        }
        return false;
    }

    @Override
    public boolean checkIfConsolContainersExist(ConsolidationDetails consolidationDetails) {
        boolean res = false;
        if(consolidationDetails.getContainersList() != null) {
            res = true;
            for(var c : consolidationDetails.getContainersList()) {
                res = res && (!Objects.isNull(c.getContainerNumber()));
            }
        }
        return res;
    }

    @Override
    public UniversalTrackingPayload mapConsoleDataToTrackingServiceData(ConsolidationDetails consolidationDetails) {
        UniversalTrackingPayload trackingPayload = null;
        if(consolidationDetails != null) {
            ShipmentDetails shipment = GetShipmentIfConsolAttached(consolidationDetails);
            trackingPayload = mapDetailsToTSData(consolidationDetails, shipment, false);
            log.info("Consolidation tracking payload : {}", trackingPayload);
        }
        return trackingPayload;
    }

    private ShipmentDetails GetShipmentIfConsolAttached(ConsolidationDetails consolidationDetails) {
        ShipmentDetails shipmentDetails = null;
        List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByConsolidationId(consolidationDetails.getId());
        if(consoleShipmentMappings != null && consoleShipmentMappings.size() > 0) {
            Long shipmentId = consoleShipmentMappings.get(0).getShipmentId();
            Optional<ShipmentDetails> optional = shipmentDao.findById(shipmentId);
            shipmentDetails = optional.get();
        }
        return shipmentDetails;
    }

    @Override
    public UniversalTrackingPayload mapShipmentDataToTrackingServiceData(ShipmentDetails shipmentDetails) {
        UniversalTrackingPayload trackingPayload = null;
        ConsolidationDetails consol = getConsolidationFromShipment(shipmentDetails.getId());
        trackingPayload = mapDetailsToTSData(consol, shipmentDetails, true);
        log.info("Shipment Tracking Update: {}", trackingPayload.toString());
        return trackingPayload;
    }

    @Override
    public List<Events> getAllEvents(ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails, String refNumber) {
        // Modified Logic (currently returning only shipment/ consol events)
        // This will be replaced once the below to-do item is completed
        List<Events> allEvents = new ArrayList<>();
        if (shipmentDetails != null) {
            allEvents.addAll(shipmentDetails.getEventsList() != null ? shipmentDetails.getEventsList() : Collections.emptyList());
            // Fetch Consol events based on ref number
            if(StringUtility.isNotEmpty(refNumber)) {
                allEvents.addAll(getEventsFromConsolidation(refNumber));
            }
        }
        if(consolidationDetails != null) {
            allEvents.addAll(consolidationDetails.getEventsList() != null ? consolidationDetails.getEventsList() : Collections.emptyList());
            // Fetch shipment events based on ref number
            allEvents.addAll(getEventsFromShipment(refNumber));
        }

        allEvents = allEvents.stream().filter(i -> (i.getIsPublicTrackingEvent() == null || !i.getIsPublicTrackingEvent())).toList();
        return allEvents;
    }

    private List<Events> getEventsFromShipment(String refNumber) {
        ListCommonRequest listCommonRequest = constructListCommonRequest("bookingReference", refNumber, "=");
        Pair<Specification<ShipmentDetails>, Pageable> pair = fetchData(listCommonRequest, ShipmentDetails.class);
        Page<ShipmentDetails> shipmentDetailsPage = shipmentDao.findAll(pair.getLeft(), pair.getRight());
        if(!shipmentDetailsPage.isEmpty()) {
            return shipmentDetailsPage.getContent().get(0).getEventsList();
        }
        return Collections.emptyList();
    }

    private List<Events> getEventsFromConsolidation(String refNumber) {
        ListCommonRequest listCommonRequest = constructListCommonRequest("referenceNumber", refNumber, "=");
        Pair<Specification<ConsolidationDetails>, Pageable> pair = fetchData(listCommonRequest, ConsolidationDetails.class);
        Page<ConsolidationDetails> consolidationDetailsPage = consolidationDetailsDao.findAll(pair.getLeft(), pair.getRight());
        if(!consolidationDetailsPage.isEmpty()) {
            return consolidationDetailsPage.getContent().get(0).getEventsList();
        }
        return Collections.emptyList();
    }

    private ConsolidationDetails getConsolidationFromShipment(Long shipmentId) {
        ConsolidationDetails consolidationDetails = null;
        try{
            List<ConsoleShipmentMapping> linkedConsol = consoleShipmentMappingDao.findByShipmentId(shipmentId);
            if(linkedConsol != null && linkedConsol.size() > 0) {
                Optional<ConsolidationDetails> optional = consolidationDetailsDao.findById(linkedConsol.get(0).getConsolidationId());
                consolidationDetails = optional.get();
            }
        } catch (Exception e) {
            log.error("Error while fetching linked consol from shipment with id {}", shipmentId);
        }
        return  consolidationDetails;
    }

    private UniversalTrackingPayload mapDetailsToTSData(ConsolidationDetails inputConsol, ShipmentDetails inputShipment, boolean isRequestFromShipment) {
        UniversalTrackingPayload trackingPayload = null;
        List<UniversalTrackingPayload.ShipmentDetail> shipmentDetails = new ArrayList<>();
        var shipDetails = getShipmentDetails(inputShipment);

        if(shipDetails != null)
            shipmentDetails.add(shipDetails);

        var entityDetails = getEntityDetails(inputConsol, inputShipment, isRequestFromShipment);
        var consolNumber = inputConsol !=null ? inputConsol.getConsolidationNumber() : null;
        var masterBill = inputConsol !=null ? inputConsol.getBol() : (inputShipment != null ? inputShipment.getMasterBill() : null);
        var shipmentNumber =  inputShipment !=null ? inputShipment.getShipmentId() : null;


        if(!isRequestFromShipment)
            trackingPayload = mapDetailsForTracking(Constants.CONSOLIDATION, consolNumber,masterBill, shipmentDetails, entityDetails);
        else
            trackingPayload = mapDetailsForTracking(Constants.SHIPMENT, shipmentNumber,masterBill, shipmentDetails, entityDetails);

        if(inputShipment != null && inputShipment.getSource().equals("API")) {
            if(!isRequestFromShipment)
                trackingPayload.setBookingReferenceNumber(inputConsol.getReferenceNumber());
            else
                trackingPayload.setBookingReferenceNumber(inputShipment.getBookingReference());
        }
        else
            trackingPayload.setBookingReferenceNumber(null);

        if((inputConsol != null && ! inputConsol.getTransportMode().equalsIgnoreCase(Constants.TRANSPORT_MODE_AIR)) || (inputShipment != null && !inputShipment.getTransportMode().equalsIgnoreCase(Constants.TRANSPORT_MODE_AIR)))
            trackingPayload.setEntityType("Container");
        else
            trackingPayload.setEntityType("awb");

        if(inputConsol != null)
            trackingPayload.setCarrier(fetchCarrierName(inputConsol.getCarrierDetails().getShippingLine()));
        else
            trackingPayload.setCarrier(fetchCarrierName(inputShipment.getCarrierDetails().getShippingLine()));

        return trackingPayload;
    }

    private List<UniversalTrackingPayload.EntityDetail> getEntityDetails(ConsolidationDetails inputConsol, ShipmentDetails inputShipment, boolean isRequestFromShipment) {
        if(inputConsol!=null && !inputConsol.getTransportMode().equals("AIR")) {
            if(!isRequestFromShipment)
                return GetContainerDetailsFromConsol(inputConsol);
            else
                return GetContainerDetailsAttachedForShipment(inputShipment);
        }
        else if(inputShipment.getTransportMode().equals("AIR"))
                return GetAWBDetailsFromShipment(inputShipment);
        else
            return null;
    }

    private String getBillOfLading(ShipmentDetails shipmentDetails) {
        if(!IsStringNullOrEmpty(shipmentDetails.getHouseBill()))
            return shipmentDetails.getHouseBill();
        return shipmentDetails.getMasterBill();
    }

    private List<UniversalTrackingPayload.EntityDetail> GetAWBDetailsFromShipment(ShipmentDetails inputShipment) {
        List<UniversalTrackingPayload.EntityDetail> result = new ArrayList<>();
        List<Awb> awbList = awbDao.findByShipmentId(inputShipment.getId());
        result.add(UniversalTrackingPayload.EntityDetail.builder()
                .trackingNumber(inputShipment.getMasterBill())
                .allocationDate(awbList == null || awbList.size() == 0 ? null: awbList.get(0).getCreatedAt().format(DateTimeFormatter.ofPattern("yyyy-MM-dd")))
                .build());

        return result;
    }

    private List<UniversalTrackingPayload.EntityDetail> GetContainerDetailsAttachedForShipment(ShipmentDetails inputShipment) {
        return getEntityDetailsFromContainers(inputShipment.getContainersList());
    }


    private List<UniversalTrackingPayload.EntityDetail> GetContainerDetailsFromConsol(ConsolidationDetails inputConsol) {
        return getEntityDetailsFromContainers(inputConsol.getContainersList());
    }

    private List<UniversalTrackingPayload.EntityDetail> getEntityDetailsFromContainers(List<Containers> containersList) {
        if(containersList == null || containersList.size() == 0)
            return null;

        List<UniversalTrackingPayload.EntityDetail> result = new ArrayList<>();
        for (var container : containersList) {
            var entityDetail = UniversalTrackingPayload.EntityDetail.builder()
                    .trackingNumber(container.getContainerNumber())
                    .allocationDate(container.getAllocationDate() != null ? (DateTimeFormatter.ofPattern("yyyy-MM-dd").format(container.getAllocationDate())) : null)
                    .grossWeight(container.getGrossWeight())
                    .grossWeightUom(container.getGrossWeightUnit())
                    .isEmpty(container.getIsEmpty())
                    .isReefer(container.getIsReefer())
                    .isShipperOwned(container.getIsShipperOwned())
                    .containerTypeCode(container.getContainerCode())
                    .mode(container.getHblDeliveryMode())
                    .containerCount(container.getContainerCount())
                    .descriptionOfGoods(container.getDescriptionOfGoods())
                    .noofPackages(IsStringNullOrEmpty(container.getPacks()) ? null : Long.valueOf(container.getPacks()))
                    .netWeight(container.getNetWeight())
                    .netWeightUom(container.getNetWeightUnit())
                    .build();
            result.add(entityDetail);
        }
        return result;
    }

    private String fetchCarrierName(String carrier) {
        if(StringUtility.isEmpty(carrier)) return null;
        List<Object> carrierCriteria = Arrays.asList(
                List.of("ItemValue"),
                "=",
                carrier
        );
        CommonV1ListRequest carrierRequest = CommonV1ListRequest.builder().skip(0).take(0).criteriaRequests(carrierCriteria).build();
        CarrierListObject carrierListObject = new CarrierListObject();
        carrierListObject.setListObject(carrierRequest);
        Object carrierResponse = masterDataFactory.getMasterDataService().fetchCarrierMasterData(carrierListObject).getData();
        List<CarrierMasterData> carrierMasterData = jsonHelper.convertValueToList(carrierResponse, CarrierMasterData.class);
        if(carrierMasterData == null || carrierMasterData.isEmpty())
            return null;
        return carrierMasterData.get(0).getIdentifier1();
    }

    private UniversalTrackingPayload.ShipmentDetail getShipmentDetails(ShipmentDetails shipmentDetails) {
        if(shipmentDetails == null)
            return null;

        UniversalTrackingPayload.ShipmentDetail response = UniversalTrackingPayload.ShipmentDetail.builder()
                .serviceMode(shipmentDetails.getServiceType())
//                .estimatedPickupDate(shipmentDetails.EstimatedPickup != null ? ((DateTime)shipmentDetails.EstimatedPickup).Date.ToString("yyyy-MM-dd") : null)
//                .bookingCreationDate(shipmentDetails.DateofIssue != null ? ((DateTime)shipmentDetails.DateofIssue).Date.ToString("yyyy-MM-dd") : null)
                .houseBill(getBillOfLading(shipmentDetails))
                .shipmentType(shipmentDetails.getDirection())
                .build();

        if(shipmentDetails.getCarrierDetails() != null) {
            CarrierDetails cd = shipmentDetails.getCarrierDetails();
            Set<String> locationRefGuids = new HashSet<>();
            locationRefGuids.add(cd.getOrigin());
            locationRefGuids.add(cd.getDestination());
            locationRefGuids.add(cd.getOriginPort());
            locationRefGuids.add(cd.getDestinationPort());
            Map<String, UnlocationsResponse> unlocationsResponseMap = getLocationData(locationRefGuids);
            response.setOriginName(unlocationsResponseMap.get(cd.getOrigin()) != null ? unlocationsResponseMap.get(cd.getOrigin()).getName() : null);
            response.setOriginPortCode(unlocationsResponseMap.get(cd.getOriginPort()) != null ? unlocationsResponseMap.get(cd.getOriginPort()).getLocCode() : null);
            response.setDestinationName(unlocationsResponseMap.get(cd.getDestination()) != null ? unlocationsResponseMap.get(cd.getDestination()).getName() : null);
            response.setDestinationCountry(unlocationsResponseMap.get(cd.getDestination()) != null ? unlocationsResponseMap.get(cd.getDestination()).getCountry() : null);
            response.setDestinationPortCode(unlocationsResponseMap.get(cd.getDestinationPort()) != null ? unlocationsResponseMap.get(cd.getDestinationPort()).getLocCode() : null);
        }
        return response;
    }

    private UniversalTrackingPayload mapDetailsForTracking(String referenceNumberType, String runnerReferenceNumber, String masterBill, List<UniversalTrackingPayload.ShipmentDetail> shipmentDetail, List<UniversalTrackingPayload.EntityDetail> entityDetail) {
        return UniversalTrackingPayload.builder()
        .runnerReferenceNumber(runnerReferenceNumber)
        .referenceNumberType(referenceNumberType)
        .shipmentDetails(shipmentDetail)
        .entityDetails(entityDetail)
        .masterBill(masterBill)
        .build();
    }

    private Map<String, UnlocationsResponse> getLocationData(Set<String> locCodes) {
        Map<String, UnlocationsResponse> locationMap = new HashMap<>();
        if (locCodes.size() > 0) {
            List<Object> criteria = Arrays.asList(
                    Arrays.asList(EntityTransferConstants.LOCATION_SERVICE_GUID),
                    "In",
                    Arrays.asList(locCodes)
            );
            CommonV1ListRequest commonV1ListRequest = CommonV1ListRequest.builder().skip(0).take(0).criteriaRequests(criteria).build();
            V1DataResponse v1DataResponse = v1Service.fetchUnlocation(commonV1ListRequest);
            List<UnlocationsResponse> unlocationsResponse = jsonHelper.convertValueToList(v1DataResponse.entities, UnlocationsResponse.class);
            if (unlocationsResponse != null && unlocationsResponse.size() > 0) {

                for (UnlocationsResponse unlocation : unlocationsResponse) {
                    locationMap.put(unlocation.getLocationsReferenceGUID(), unlocation);
                }

            }
        }
        return locationMap;
    }

    @Override
    public TrackingServiceApiResponse fetchTrackingData(TrackingRequest request) throws RunnerException {
        var headers = new HttpHeaders();
        headers.add(ApiConstants.X_API_KEY, trackingServiceApiKey);
        headers.setContentType(MediaType.APPLICATION_JSON);
        var httpEntity = new HttpEntity<>(List.of(TrackingServiceApiRequest.builder().shipmentReference(request.getReferenceNumber()).build()), headers);

        try {
            log.info("Entered tracking call");
            var response = restTemplate.postForEntity(trackingServiceNewFlowEndpoint, httpEntity, TrackingServiceApiResponse.class);
            var responseBody = response.getBody();
            log.info("Received response from tracking {}", responseBody);
            return responseBody;
        } catch (Exception e){
            log.error("Error while calling tracking endpoint ", e);
            throw new RunnerException(e.getMessage());
        }
    }

    // WILL REMOVE AFTER EVENTS TESTING
    // IGNORE THE COMMENTED SECTION

//    @Override
//    public TrackingServiceApiResponse fetchTrackingData(TrackingRequest request) throws RunnerException {
//        // Toggle this flag to switch between remote call and reading from file
//        boolean useLocalJson = true; // Set this to true to use JSON file A
//
//        if (useLocalJson) {
//            return fetchFromJsonFile("src/main/resources/ts_payload_sample.json");
//        }
//
//        // Existing code for remote call
//        var headers = new HttpHeaders();
//        headers.add(ApiConstants.X_API_KEY, trackingServiceApiKey);
//        headers.setContentType(MediaType.APPLICATION_JSON);
//        var httpEntity = new HttpEntity<>(List.of(
//                TrackingServiceApiRequest.builder()
//                        .shipmentReference(request.getReferenceNumber())
//                        .build()), headers);
//
//        try {
//            log.info("Entered tracking call");
//            var response = restTemplate.postForEntity(
//                    trackingServiceNewFlowEndpoint,
//                    httpEntity,
//                    TrackingServiceApiResponse.class);
//            var responseBody = response.getBody();
//            log.info("Received response from tracking {}", responseBody);
//            return responseBody;
//        } catch (Exception e) {
//            log.error("Error while calling tracking endpoint ", e);
//            throw new RunnerException(e.getMessage());
//        }
//    }
//
//    // Method to read the JSON file and convert it into TrackingServiceApiResponse
//    private TrackingServiceApiResponse fetchFromJsonFile(String filePath) throws RunnerException {
//        try {
//            ObjectMapper objectMapper = new ObjectMapper();
//            objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
//            objectMapper.registerModule(new JavaTimeModule());
//            // Assuming the JSON file is on the classpath, adjust if needed
//
//            return objectMapper.readValue(new File(filePath), TrackingServiceApiResponse.class);
//        } catch (IOException e) {
//            log.error("Error reading JSON file", e);
//            throw new RunnerException("Error reading JSON file: " + e.getMessage());
//        }
//    }

    /**
     * Converts a tracking event code to a short code based on the location role.
     * <p>
     * This method translates specific tracking event codes to their corresponding short codes based on the location role. The conversion is based on predefined constants and
     * specific conditions.
     *
     * @param locationRole the role of the location associated with the event
     * @param eventCode    the original event code to be converted
     * @param description
     * @return the corresponding short code if a match is found, otherwise returns the original event code
     */
    @Override
    public String convertTrackingEventCodeToShortCode(String locationRole, String eventCode, String description) {

        String safeEventCode = StringUtils.defaultString(eventCode);
        String safeLocationRole = StringUtils.defaultString(locationRole);
        String safeDescription = StringUtils.defaultString(description);

        log.debug("Converting event code '{}' with location role '{}'", safeEventCode, safeLocationRole);

        if (EventConstants.FLIGHT_ARRIVAL.equalsIgnoreCase(safeEventCode)
                && safeDescription.equalsIgnoreCase("Flight Arrival")) {
            log.debug("Matched FLIGHT_ARRIVAL and DESCRIPTION. Returning short code: {}", EventConstants.FLAR);
            return EventConstants.FLAR;
        }

        if (EventConstants.FLIGHT_DEPARTURE.equalsIgnoreCase(safeEventCode)
                && safeDescription.equalsIgnoreCase("Flight Departure")) {
            log.debug("Matched FLIGHT_DEPARTURE and DESCRIPTION. Returning short code: {}", EventConstants.FLDR);
            return EventConstants.FLDR;
        }

        if (EventConstants.LITERAL.equalsIgnoreCase(safeEventCode)) {
            String shortCode = null;

            if (safeDescription.equalsIgnoreCase("Received from Flight")) {
                shortCode = EventConstants.TRCF;
            } else if (safeDescription.equalsIgnoreCase("Consignee notified")) {
                shortCode = EventConstants.TNFD;
            } else if (safeDescription.equalsIgnoreCase("Received from Shipper")) {
                shortCode = EventConstants.TRCS;
            }

            if (shortCode != null) {
                log.debug("Matched LITERAL and DESCRIPTION. Returning short code: {}", shortCode);
                return shortCode;
            }
        }

        if (EventConstants.GATE_IN_WITH_CONTAINER_EMPTY.equalsIgnoreCase(safeEventCode)
                && safeLocationRole.startsWith(EventConstants.ORIGIN)) {
            log.debug("Matched GATE_IN_WITH_CONTAINER_EMPTY and ORIGIN. Returning short code: {}", EventConstants.ECPK);
            return EventConstants.ECPK;
        }

        if (EventConstants.GATE_IN_WITH_CONTAINER_FULL.equalsIgnoreCase(safeEventCode)
                && "originPort".equalsIgnoreCase(safeLocationRole)) {
            log.debug("Matched GATE_IN_WITH_CONTAINER_FULL and originPort. Returning short code: {}", EventConstants.FCGI);
            return EventConstants.FCGI;
        }

        if (EventConstants.VESSEL_DEPARTURE_WITH_CONTAINER.equalsIgnoreCase(safeEventCode)
                && "originPort".equalsIgnoreCase(safeLocationRole)) {
            log.debug("Matched VESSEL_DEPARTURE_WITH_CONTAINER and originPort. Returning short code: {}", EventConstants.VSDP);
            return EventConstants.VSDP;
        }

        if (EventConstants.VESSEL_ARRIVAL_WITH_CONTAINER.equalsIgnoreCase(safeEventCode)
                && "destinationPort".equalsIgnoreCase(safeLocationRole)) {
            log.debug("Matched VESSEL_ARRIVAL_WITH_CONTAINER and destinationPort. Returning short code: {}", EventConstants.ARDP);
            return EventConstants.ARDP;
        }

        if (EventConstants.GATE_OUT_WITH_CONTAINER_FULL.equalsIgnoreCase(safeEventCode)
                && "destinationPort".equalsIgnoreCase(safeLocationRole)) {
            log.debug("Matched GATE_OUT_WITH_CONTAINER_FULL and destinationPort. Returning short code: {}", EventConstants.FUGO);
            return EventConstants.FUGO;
        }

        if (EventConstants.GATE_IN_WITH_CONTAINER_EMPTY.equalsIgnoreCase(safeEventCode)
                && safeLocationRole.startsWith(EventConstants.DESTINATION)) {
            log.debug("Matched GATE_IN_WITH_CONTAINER_EMPTY and DESTINATION. Returning short code: {}", EventConstants.EMCR);
            return EventConstants.EMCR;
        }

        log.debug("No match found for event code '{}' with location role '{}'. Returning original event code.", safeEventCode, safeLocationRole);
        return eventCode;
    }

    @Override
    public TrackingEventsResponse getTrackingEventsResponse(String referenceNumber) throws RunnerException {
        try {
            TrackingEventsResponse trackingEventsResponse = new TrackingEventsResponse();
            var res = fetchTrackingData(TrackingRequest.builder().referenceNumber(referenceNumber).build());
            ConcurrentLinkedQueue<Events> eventsRows = new ConcurrentLinkedQueue<>();
            ForkJoinPool customThreadPool = new ForkJoinPool(4);

            try {
                if (res.getContainers() != null && !res.getContainers().isEmpty()) {
                    customThreadPool.submit(() ->
                            res.getContainers().parallelStream().forEach(container -> {
                                if (container == null || container.getEvents() == null || container.getPlaces() == null) {
                                    return;
                                }

                                // Mapping container events to Events list without processing sources
                                List<Events> rows = container.getEvents().stream()
                                        .filter(Objects::nonNull)
                                        .map(event -> {
                                            // Start building the Events object
                                            Events.EventsBuilder eventBuilder = Events.builder()
                                                    .actual(event.getActualEventTime() != null ? event.getActualEventTime().getDateTime() : null)
                                                    .estimated(event.getProjectedEventTime() != null ? event.getProjectedEventTime().getDateTime() : null)
                                                    .eventCode(convertTrackingEventCodeToShortCode(event.getLocationRole(), event.getEventType(), event.getDescriptionFromSource()))
                                                    .description(event.getDescriptionFromSource())
                                                    .containerNumber(container.getContainerNumber())
                                                    .locationRole(event.getLocationRole());

                                            // Populate fields from Places independently
                                            container.getPlaces().stream()
                                                    .filter(place -> place != null && event.getLocation() != null && event.getLocation().equals(place.getId()))
                                                    .findFirst()
                                                    .ifPresent(place -> eventBuilder.latitude(place.getLatitude())
                                                            .longitude(place.getLongitude())
                                                            .placeDescription(place.getFormattedDescription())
                                                            .placeName(place.getName()));

                                            // Populate fields from Transports independently
                                            container.getTransports().stream()
                                                    .filter(transport -> transport != null && event.getLocation() != null && event.getDetails().getTransport()
                                                            .equals(transport.getId()))
                                                    .findFirst()
                                                    .ifPresent(transport -> eventBuilder.flightNumber(transport.getName())
                                                            .flightName(String.valueOf(transport.getOperatorName())));

                                            // Build and return the event
                                            return eventBuilder.build();
                                        })
                                        .toList();

                                eventsRows.addAll(rows);

                            })
                    ).join();

                    // Set ATA and ATD date based on container journey details
                    var container = res.getContainers().get(0);
                    if (container.getJourney() != null) {
                        trackingEventsResponse.setShipmentAta(Optional.ofNullable(container.getJourney().getPortOfArrivalAta())
                                .map(TrackingServiceApiResponse.DateAndSources::getDateTime).orElse(null));
                        trackingEventsResponse.setShipmentAtd(Optional.ofNullable(container.getJourney().getPortOfDepartureAtd())
                                .map(TrackingServiceApiResponse.DateAndSources::getDateTime).orElse(null));
                    }
                }
            } finally {
                customThreadPool.shutdown();
            }

            trackingEventsResponse.setEventsList(eventsRows.stream().toList());
            return trackingEventsResponse;

        } catch (Exception e) {
            throw new RunnerException(e.getMessage());
        }
    }

    private List<TrackingServiceApiResponse.Source> getDefaultListValue (List<TrackingServiceApiResponse.Source> lst) {
        if(lst == null)
            return Collections.emptyList();
        return lst;
    }
}
