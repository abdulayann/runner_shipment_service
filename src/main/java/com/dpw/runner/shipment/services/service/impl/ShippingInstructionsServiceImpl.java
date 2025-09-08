package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.commons.constants.ShippingInstructionsConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.ICarrierBookingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShippingInstructionDao;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.ShippingInstructionRequest;
import com.dpw.runner.shipment.services.dto.response.FieldClassDto;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.ShippingInstructionResponse;
import com.dpw.runner.shipment.services.entity.CarrierBooking;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.SailingInformation;
import com.dpw.runner.shipment.services.entity.ShippingInstruction;
import com.dpw.runner.shipment.services.entity.enums.*;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.helpers.ShippingInstructionMasterDataHelper;
import com.dpw.runner.shipment.services.kafka.dto.GenericKafkaPayload;
import com.dpw.runner.shipment.services.kafka.producer.KafkaProducer;
import com.dpw.runner.shipment.services.service.interfaces.IShippingInstructionsService;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.FieldUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;

import static com.dpw.runner.shipment.services.commons.constants.CarrierBookingConstants.*;
import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@Service
@Slf4j
public class ShippingInstructionsServiceImpl implements IShippingInstructionsService {

    @Autowired
    private IShippingInstructionDao repository;

    @Autowired
    ICarrierBookingDao carrierBookingDao;

    @Autowired
    IConsolidationDetailsDao consolidationDetailsDao;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private MasterDataUtils masterDataUtils;

    @Autowired
    @Qualifier("executorServiceMasterData")
    ExecutorService executorServiceMasterData;

    @Autowired
    private CommonUtils commonUtils;

    @Autowired
    private KafkaProducer producer;

    @Autowired
    ShippingInstructionMasterDataHelper shippingInstructionMasterDataHelper;

    //@Value("${booking.queue}")
    private String bookingQueue = "";


    private final ObjectMapper objectMapper = new ObjectMapper();

    public ShippingInstructionResponse createShippingInstruction(ShippingInstructionRequest info) {
        ShippingInstruction shippingInstruction = jsonHelper.convertValue(info, ShippingInstruction.class);
        validateFetchAndSetSI(shippingInstruction);

        shippingInstruction.setStatus(ShippingInstructionStatus.Draft.name());
        ShippingInstruction savedInfo = repository.save(shippingInstruction);
        //sendDataToKafka(savedInfo);
        return jsonHelper.convertValue(savedInfo, ShippingInstructionResponse.class);
    }

    private void validateFetchAndSetSI(ShippingInstruction shippingInstruction) {
        validateSIRequest(shippingInstruction);
        if (shippingInstruction.getEntityType().toString().equalsIgnoreCase(ShippingInstructionEntityType.CARRIER_BOOKING.name())) {
            Optional<CarrierBooking> carrierBooking = carrierBookingDao.findById(shippingInstruction.getEntityId());
            Optional<ConsolidationDetails> consolidationDetail;
            if (carrierBooking.isPresent()) {
                consolidationDetail = getConsolidationDetail(carrierBooking.get().getEntityId());
                consolidationDetail.ifPresent(consolidationDetails -> fillDetailsFromConsol(shippingInstruction, consolidationDetails));
                populateHeaderSection(shippingInstruction, carrierBooking.get());
            }
        } else if (shippingInstruction.getEntityType().toString().equalsIgnoreCase(ShippingInstructionEntityType.CONSOLIDATION.name())) {
            Optional<ConsolidationDetails> consolidationDetail = getConsolidationDetail(shippingInstruction.getEntityId());
            consolidationDetail.ifPresent(consolidationDetails -> fillDetailsFromConsol(shippingInstruction, consolidationDetails));
        } else {
            throw new ValidationException("Invalid value of Shipping Instruction Type");
        }
    }

    private Optional<ConsolidationDetails> getConsolidationDetail(Long id) {
        return consolidationDetailsDao.findById(id);
    }

    public ShippingInstructionResponse getShippingInstructionsById(Long id) {
        Optional<ShippingInstruction> shippingInstruction = repository.findById(id);
        if (shippingInstruction.isEmpty()) {
            log.debug("SI is null for Id {}", id);
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        return jsonHelper.convertValue(shippingInstruction, ShippingInstructionResponse.class);
    }

    public ShippingInstructionResponse updateShippingInstructions(ShippingInstructionRequest updatedInfo) {
        ShippingInstruction shippingInstruction = jsonHelper.convertValue(updatedInfo, ShippingInstruction.class);
        validateFetchAndSetSI(shippingInstruction);
        ShippingInstruction information = repository.save(shippingInstruction);
        sendDataToKafka(information);
        return jsonHelper.convertValue(information, ShippingInstructionResponse.class);
    }

    public void deleteShippingInstructions(Long id) {
        repository.delete(id);
    }

    public ResponseEntity<IRunnerResponse> getAllMasterData(Long shippingInstId) {
        String responseMsg;
        try {
            Optional<ShippingInstruction> shippingInstructionOpt = repository.findById(shippingInstId);
            if (shippingInstructionOpt.isEmpty()) {
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            ShippingInstruction carrierBooking = shippingInstructionOpt.get();
            long start = System.currentTimeMillis();
            List<String> includeColumns = FieldUtils.getMasterDataAnnotationFields(List.of(createFieldClassDto(ShippingInstruction.class, null), createFieldClassDto(SailingInformation.class, "sailingInformation.")));
            includeColumns.addAll(ShippingInstructionsConstants.LIST_INCLUDE_COLUMNS);
            ShippingInstructionResponse shippingInstructionResponse = (ShippingInstructionResponse) commonUtils.setIncludedFieldsToResponse(carrierBooking, new HashSet<>(includeColumns), new ShippingInstructionResponse());
            log.info("Total time taken in setting carrier booking details response {}", (System.currentTimeMillis() - start));
            Map<String, Object> response = fetchAllMasterDataByKey(shippingInstructionResponse);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_DATA_RETRIEVAL_FAILURE;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel, boolean getMasterData) {
        ListCommonRequest listCommonRequest = (ListCommonRequest) commonRequestModel.getData();
        if (listCommonRequest == null) {
            log.error(CARRIER_LIST_REQUEST_EMPTY_ERROR, LoggerHelper.getRequestIdFromMDC());
            throw new ValidationException(CARRIER_LIST_REQUEST_NULL_ERROR);
        }
        if (listCommonRequest.getIncludeColumns() == null || listCommonRequest.getIncludeColumns().isEmpty()) {
            throw new ValidationException(CARRIER_INCLUDE_COLUMNS_REQUIRED_ERROR_MESSAGE);
        }

        Pair<Specification<ShippingInstruction>, Pageable> tuple = fetchData(listCommonRequest, ShippingInstruction.class, ShippingInstructionsConstants.tableNames);
        Page<ShippingInstruction> carrierBookingPage = repository.findAll(tuple.getLeft(), tuple.getRight());
        log.info(CARRIER_LIST_RESPONSE_SUCCESS, LoggerHelper.getRequestIdFromMDC());


        List<IRunnerResponse> filteredList = convertEntityListToDtoList(carrierBookingPage.getContent(), getMasterData, new HashSet<>(listCommonRequest.getIncludeColumns()));

        return ResponseHelper.buildListSuccessResponse(
                filteredList,
                carrierBookingPage.getTotalPages(),
                carrierBookingPage.getTotalElements());
    }

    private void fillDetailsFromConsol(ShippingInstruction shippingInstruction, ConsolidationDetails consolidationDetails) {
        setSailingInfoAndCutoff(shippingInstruction, consolidationDetails);
    }

    private void setSailingInfoAndCutoff(ShippingInstruction shippingInstruction, ConsolidationDetails consolidationDetails) {
        if (shippingInstruction.getSailingInformation() != null) {
            shippingInstruction.getSailingInformation().setCarrierReceiptPlace(consolidationDetails.getCarrierDetails().getOrigin());
            shippingInstruction.getSailingInformation().setPol(consolidationDetails.getCarrierDetails().getOriginPort());
            shippingInstruction.getSailingInformation().setPod(consolidationDetails.getCarrierDetails().getDestinationPort());
            shippingInstruction.getSailingInformation().setCarrierDeliveryPlace(consolidationDetails.getCarrierDetails().getDestination());
            //    shippingInstruction.getSailingInformation().setCarrier(consolidationDetails.getCarrierDetails().get);
            shippingInstruction.getSailingInformation().setShipInstructionCutoff(consolidationDetails.getShipInstructionCutoff());
            shippingInstruction.getSailingInformation().setVerifiedGrossMassCutoff(consolidationDetails.getVerifiedGrossMassCutoff());
        }
    }

    private void populateHeaderSection(ShippingInstruction shippingInstruction, CarrierBooking carrierBooking) {
        shippingInstruction.setStatus(carrierBooking.getStatus().toString());
        shippingInstruction.setCarrierBookingNo(carrierBooking.getCarrierBookingNo());
        shippingInstruction.setCarrierBlNo(carrierBooking.getCarrierBlNo());
        shippingInstruction.setServiceType(carrierBooking.getServiceType());
    }

    private void validateSIRequest(ShippingInstruction shippingInstruction) {
        if (Integer.parseInt(String.valueOf(shippingInstruction.getNoOfFreightCopies())) > 100
                || Integer.parseInt(String.valueOf(shippingInstruction.getNoOfFreightCopies())) < 0) {
            log.info("Validation failed for number of freight copies for SI id : {}", shippingInstruction.getId());
            throw new ValidationException("Invalid freight copies number!");
        }

        if (Integer.parseInt(String.valueOf(shippingInstruction.getNonNegoFreightCopies())) > 100
                || Integer.parseInt(String.valueOf(shippingInstruction.getNonNegoFreightCopies())) < 0) {
            log.info("Validation failed for getNonNegoFreightCopies for SI id : {}", shippingInstruction.getId());
            throw new ValidationException("Invalid getNonNegoFreightCopies!");
        }

        if (Integer.parseInt(String.valueOf(shippingInstruction.getNoOfUnFreightCopies())) > 100
                || Integer.parseInt(String.valueOf(shippingInstruction.getNoOfUnFreightCopies())) < 0) {
            log.info("Validation failed for getNoOfUnFreightCopies for SI id : {}", shippingInstruction.getId());
            throw new ValidationException("Invalid un freight copies number!");
        }

        if (Integer.parseInt(String.valueOf(shippingInstruction.getNonNegoUnFreightCopies())) > 100
                || Integer.parseInt(String.valueOf(shippingInstruction.getNonNegoUnFreightCopies())) < 0) {
            log.info("Validation failed for getNonNegoUnFreightCopies for SI id : {}", shippingInstruction.getId());
            throw new ValidationException("Invalid getNonNegoUnFreightCopies!");
        }
    }

    private FieldClassDto createFieldClassDto(Class<?> clazz, String parentref) {
        FieldClassDto fieldClassDto = new FieldClassDto();
        fieldClassDto.setClazz(clazz);
        fieldClassDto.setFieldRef(parentref);
        return fieldClassDto;
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<ShippingInstruction> shippingInstructionList, boolean getMasterData,
                                                             Set<String> includeColumns) {
        List<ShippingInstructionResponse> shippingInstructionResponses = new ArrayList<>();

        for (ShippingInstruction shippingInstruction : shippingInstructionList) {
            ShippingInstructionResponse shippingInstructionResponse = jsonHelper.convertValue(shippingInstruction, ShippingInstructionResponse.class);
            shippingInstructionResponses.add(shippingInstructionResponse);
        }

        List<IRunnerResponse> responseList = new ArrayList<>(shippingInstructionResponses);
        getMasterDataForList(shippingInstructionList, responseList, getMasterData, true, includeColumns);
        return responseList;
    }

    public void getMasterDataForList(List<ShippingInstruction> lst, List<IRunnerResponse> responseList, boolean getMasterData, boolean includeTenantData, Set<String> includeColumns) {
        if (getMasterData) {
            try {
                double startTime = System.currentTimeMillis();
                var locationDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.setLocationData(responseList, EntityTransferConstants.LOCATION_SERVICE_GUID)), executorServiceMasterData);
                var vesselDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.fetchVesselForList(responseList)), executorServiceMasterData);
                var carrierDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.fetchCarriersForList(responseList)), executorServiceMasterData);

                CompletableFuture.allOf(locationDataFuture, vesselDataFuture, carrierDataFuture).join();
                log.info("Time taken to fetch Master-data for event:{} | Time: {} ms. || RequestId: {}", LoggerEvent.SHIPMENT_LIST_MASTER_DATA, (System.currentTimeMillis() - startTime), LoggerHelper.getRequestIdFromMDC());
            } catch (Exception ex) {
                log.error(Constants.ERROR_OCCURRED_FOR_EVENT, LoggerHelper.getRequestIdFromMDC(), IntegrationType.MASTER_DATA_FETCH_FOR_SHIPMENT_LIST, ex.getLocalizedMessage());
            }
        }
    }

    public Map<String, Object> fetchAllMasterDataByKey(ShippingInstructionResponse shippingInstructionResponse) {
        Map<String, Object> masterDataResponse = new HashMap<>();
        var masterListFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> shippingInstructionMasterDataHelper.addAllMasterDataInSingleCall(shippingInstructionResponse, masterDataResponse)), executorServiceMasterData);
        var unLocationsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> shippingInstructionMasterDataHelper.addAllUnlocationDataInSingleCall(shippingInstructionResponse, masterDataResponse)), executorServiceMasterData);
        var carrierFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> shippingInstructionMasterDataHelper.addAllCarrierDataInSingleCall(shippingInstructionResponse, masterDataResponse)), executorServiceMasterData);
        var vesselsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> shippingInstructionMasterDataHelper.addAllVesselDataInSingleCall(shippingInstructionResponse, masterDataResponse)), executorServiceMasterData);
        CompletableFuture.allOf(masterListFuture, unLocationsFuture, carrierFuture, vesselsFuture).join();
        return masterDataResponse;
    }

    private void sendDataToKafka(ShippingInstruction shippingInstruction) {
        try {
            // Convert object to JSON string
            String jsonPayload = objectMapper.writeValueAsString(shippingInstruction);

            // Wrap it inside your generic payload
            GenericKafkaPayload genericKafkaMsg =
                    new GenericKafkaPayload(GenericKafkaMsgType.SI, jsonPayload);

            log.debug("SI Payload sent to kafka with id {}",genericKafkaMsg.getId());
            producer.produceToKafka(genericKafkaMsg, bookingQueue, genericKafkaMsg.getId());

        } catch (JsonProcessingException e) {
            throw new RuntimeException("Error serializing ShippingInstruction", e);
        }
    }
}
