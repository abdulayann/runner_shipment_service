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
import com.dpw.runner.shipment.services.entity.CommonContainers;
import com.dpw.runner.shipment.services.entity.CommonPackages;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.SailingInformation;
import com.dpw.runner.shipment.services.entity.ShippingInstruction;
import com.dpw.runner.shipment.services.entity.enums.GenericKafkaMsgType;
import com.dpw.runner.shipment.services.entity.enums.IntegrationType;
import com.dpw.runner.shipment.services.entity.enums.LoggerEvent;
import com.dpw.runner.shipment.services.entity.enums.ShippingInstructionEntityType;
import com.dpw.runner.shipment.services.entity.enums.ShippingInstructionStatus;
import com.dpw.runner.shipment.services.entity.enums.ShippingInstructionType;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.helpers.ShippingInstructionMasterDataHelper;
import com.dpw.runner.shipment.services.kafka.dto.GenericKafkaPayload;
import com.dpw.runner.shipment.services.kafka.producer.KafkaProducer;
import com.dpw.runner.shipment.services.service.interfaces.IPackingV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IShippingInstructionsService;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.FieldUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.commons.constants.CarrierBookingConstants.CARRIER_INCLUDE_COLUMNS_REQUIRED_ERROR_MESSAGE;
import static com.dpw.runner.shipment.services.commons.constants.CarrierBookingConstants.CARRIER_LIST_REQUEST_EMPTY_ERROR;
import static com.dpw.runner.shipment.services.commons.constants.CarrierBookingConstants.CARRIER_LIST_REQUEST_NULL_ERROR;
import static com.dpw.runner.shipment.services.commons.constants.CarrierBookingConstants.CARRIER_LIST_RESPONSE_SUCCESS;
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
    IPackingV3Service packingV3Service;

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


    public ShippingInstructionResponse createShippingInstruction(ShippingInstructionRequest info) {
        ShippingInstruction shippingInstruction = jsonHelper.convertValue(info, ShippingInstruction.class);
        validateFetchAndSetSI(shippingInstruction, true);

        shippingInstruction.setStatus(ShippingInstructionStatus.Draft.name());
        ShippingInstruction savedInfo = repository.save(shippingInstruction);
        return jsonHelper.convertValue(savedInfo, ShippingInstructionResponse.class);
    }

    private void validateFetchAndSetSI(ShippingInstruction shippingInstruction, boolean isCreate) {
        validateSIRequest(shippingInstruction);
        ConsolidationDetails consolidationDetails;
        if (ShippingInstructionEntityType.CARRIER_BOOKING.equals(shippingInstruction.getEntityType())) {
            Optional<CarrierBooking> carrierBooking = carrierBookingDao.findById(shippingInstruction.getEntityId());
            if (carrierBooking.isEmpty()) {
                throw new ValidationException("Invalid entity id");
            }
            populateHeaderSection(shippingInstruction, carrierBooking.get());
            populateSailingInformationFromCarrierBooking(shippingInstruction, carrierBooking.get());
            consolidationDetails = getConsolidationDetail(carrierBooking.get().getEntityId());

        } else if (ShippingInstructionEntityType.CONSOLIDATION.equals(shippingInstruction.getEntityType())) {
            consolidationDetails = getConsolidationDetail(shippingInstruction.getEntityId());
            fillDetailsFromConsol(shippingInstruction, consolidationDetails);
        } else {
            throw new ValidationException("Invalid value of Shipping Instruction Type");
        }
        if (isCreate) {
            List<Packing> packingList = packingV3Service.getPackingsByConsolidationId(consolidationDetails.getId());
            List<Containers> containersList = consolidationDetails.getContainersList();
            Map<Long, Containers> containersMap = new HashMap<>();
            if (!CollectionUtils.isEmpty(containersList)) {
                containersMap = containersList.stream()
                        .collect(Collectors.toMap(
                                Containers::getId,
                                Function.identity(),
                                (existing, replacement) -> existing
                        ));
            }
            shippingInstruction.setCommonPackagesList(setCommonPackages(packingList, containersMap));
            shippingInstruction.setCommonContainersList(setCommonContainers(containersList));
        }

    }

    private List<CommonContainers> setCommonContainers(List<Containers> containersList) {
        List<CommonContainers> commonContainersList = new ArrayList<>();
        if (!CollectionUtils.isEmpty(containersList)) {
            for (Containers containers : containersList) {
                CommonContainers commonContainers = new CommonContainers();
                commonContainers.setContainerCode(containers.getContainerCode());
                commonContainers.setContainerNo(containers.getContainerNumber());
                commonContainers.setPacks(StringUtility.isNotEmpty(containers.getPacks()) ? Integer.parseInt(containers.getPacks()) : null);
                commonContainers.setPacksUnit(containers.getPacksType());
                commonContainers.setHsCode(containers.getHsCode());
                commonContainers.setCommodityCode(containers.getCommodityCode());
                commonContainers.setCommodityGroup(containers.getCommodityGroup());
                commonContainers.setMarksNums(containers.getMarksNums());
                commonContainers.setGoodsDescription(containers.getDescriptionOfGoods());
                commonContainers.setGrossWeight(containers.getGrossWeight());
                commonContainers.setGrossWeightUnit(containers.getGrossWeightUnit());
                commonContainers.setVolume(containers.getGrossVolume());
                commonContainers.setVolumeUnit(containers.getGrossVolumeUnit());
                commonContainers.setNetWeight(containers.getNetWeight());
                commonContainers.setNetWeightUnit(containers.getNetWeightUnit());
                commonContainers.setTareWeight(containers.getTareWeight());
                commonContainers.setTareWeightUnit(containers.getTareWeightUnit());
                commonContainers.setCustomsSealNumber(containers.getCustomsSealNumber());
                commonContainers.setShipperSealNumber(containers.getShipperSealNumber());
                commonContainers.setVeterinarySealNumber(containers.getVeterinarySealNumber());
                commonContainersList.add(commonContainers);
            }
        }
        return commonContainersList;
    }

    private List<CommonPackages> setCommonPackages(List<Packing> packingList, Map<Long, Containers> containersMap) {

        List<CommonPackages> commonPackagesList = new ArrayList<>();
        if (!CollectionUtils.isEmpty(packingList)) {
            for (Packing packing : packingList) {
                CommonPackages commonPackages = new CommonPackages();
                commonPackages.setPacks(StringUtility.isNotEmpty(packing.getPacks()) ? Integer.parseInt(packing.getPacks()) : null);
                commonPackages.setPacksUnit(packing.getPacksType());
                commonPackages.setHsCode(packing.getHSCode());
                commonPackages.setGrossWeight(packing.getWeight());
                commonPackages.setGrossWeightUnit(packing.getWeightUnit());
                commonPackages.setVolume(packing.getVolume());
                commonPackages.setVolumeUnit(packing.getVolumeUnit());
                commonPackages.setGoodsDescription(packing.getGoodsDescription());
                commonPackages.setCommodityCode(packing.getCommodity());
                commonPackages.setCommodityGroup(packing.getCommodityGroup());
                commonPackages.setMarksnNums(packing.getMarksnNums());
                Containers containers = containersMap.get(packing.getContainerId());
                if (Objects.nonNull(containers)) {
                    commonPackages.setContainerNo(containers.getContainerNumber());
                }
                commonPackagesList.add(commonPackages);
            }
        }
        return commonPackagesList;
    }

    private void populateSailingInformationFromCarrierBooking(ShippingInstruction shippingInstruction, CarrierBooking carrierBooking) {
        if (Objects.nonNull(shippingInstruction.getSailingInformation())) {
            shippingInstruction.getSailingInformation().setCarrierReceiptPlace(carrierBooking.getSailingInformation().getCarrierReceiptPlace());
            shippingInstruction.getSailingInformation().setPol(carrierBooking.getSailingInformation().getPol());
            shippingInstruction.getSailingInformation().setPod(carrierBooking.getSailingInformation().getPod());
            shippingInstruction.getSailingInformation().setCarrierDeliveryPlace(carrierBooking.getSailingInformation().getCarrierDeliveryPlace());
            shippingInstruction.getSailingInformation().setCarrier(carrierBooking.getSailingInformation().getCarrier());
            shippingInstruction.getSailingInformation().setShipInstructionCutoff(carrierBooking.getSailingInformation().getShipInstructionCutoff());
            shippingInstruction.getSailingInformation().setVerifiedGrossMassCutoff(carrierBooking.getSailingInformation().getVerifiedGrossMassCutoff());
        }
    }

    private ConsolidationDetails getConsolidationDetail(Long id) {
        Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findById(id);
        if (consolidationDetails.isEmpty()) {
            throw new ValidationException("Consolidation details does not exist " + id);
        }
        return consolidationDetails.get();
    }

    public ShippingInstructionResponse getShippingInstructionsById(Long id) {
        Optional<ShippingInstruction> shippingInstruction = repository.findById(id);
        if (shippingInstruction.isEmpty()) {
            log.debug("SI is null for Id {}", id);
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        return jsonHelper.convertValue(shippingInstruction, ShippingInstructionResponse.class);
    }

    public ShippingInstructionResponse updateShippingInstructions(ShippingInstructionRequest shippingInstructionRequest) {
        Optional<ShippingInstruction> shippingInstructionEntity = repository.findById(shippingInstructionRequest.getId());
        if (shippingInstructionEntity.isEmpty()) {
            throw new ValidationException("Invalid shipping instruction id");
        }
        ShippingInstruction shippingInstruction = jsonHelper.convertValue(shippingInstructionRequest, ShippingInstruction.class);
        validateFetchAndSetSI(shippingInstruction, false);
        shippingInstruction.setCommonContainersList(shippingInstructionEntity.get().getCommonContainersList());
        shippingInstruction.setCommonPackagesList(shippingInstructionEntity.get().getCommonPackagesList());
        ShippingInstruction information = repository.save(shippingInstruction);
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
        shippingInstruction.setEntityNumber(consolidationDetails.getConsolidationNumber());
        setSailingInfoAndCutoff(shippingInstruction, consolidationDetails);
    }

    private void setSailingInfoAndCutoff(ShippingInstruction shippingInstruction, ConsolidationDetails consolidationDetails) {
        if (shippingInstruction.getSailingInformation() != null) {
            shippingInstruction.getSailingInformation().setCarrierReceiptPlace(consolidationDetails.getCarrierDetails().getOrigin());
            shippingInstruction.getSailingInformation().setPol(consolidationDetails.getCarrierDetails().getOriginPort());
            shippingInstruction.getSailingInformation().setPod(consolidationDetails.getCarrierDetails().getDestinationPort());
            shippingInstruction.getSailingInformation().setCarrierDeliveryPlace(consolidationDetails.getCarrierDetails().getDestination());
            shippingInstruction.getSailingInformation().setShipInstructionCutoff(consolidationDetails.getShipInstructionCutoff());
            shippingInstruction.getSailingInformation().setVerifiedGrossMassCutoff(consolidationDetails.getVerifiedGrossMassCutoff());
        }
    }

    private void populateHeaderSection(ShippingInstruction shippingInstruction, CarrierBooking carrierBooking) {
        shippingInstruction.setCarrierBookingNo(carrierBooking.getCarrierBookingNo());
        shippingInstruction.setCarrierBlNo(carrierBooking.getCarrierBlNo());
        shippingInstruction.setEntityNumber(carrierBooking.getBookingNo());
    }

    private void validateSIRequest(ShippingInstruction shippingInstruction) {
        if (Objects.nonNull(shippingInstruction.getNoOfFreightCopies()) && (shippingInstruction.getNoOfFreightCopies() > 100
                || shippingInstruction.getNoOfFreightCopies() < 0)) {
            log.info("Validation failed for number of freight copies for SI id : {}", shippingInstruction.getId());
            throw new ValidationException("Invalid freight copies number!");
        }
        if (Objects.nonNull(shippingInstruction.getNoOfUnFreightCopies()) && (shippingInstruction.getNoOfUnFreightCopies() > 100
                || shippingInstruction.getNoOfUnFreightCopies() < 0)) {
            log.info("Validation failed for getNoOfUnFreightCopies for SI id : {}", shippingInstruction.getId());
            throw new ValidationException("Invalid un freight copies number!");
        }
        if (ShippingInstructionType.EXPRESS.equals(shippingInstruction.getShippingInstructionType())) {
            if (Objects.nonNull(shippingInstruction.getNonNegoFreightCopies()) && (shippingInstruction.getNonNegoFreightCopies() > 100
                    || shippingInstruction.getNonNegoFreightCopies() < 0)) {
                log.info("Validation failed for getNonNegoFreightCopies for SI id : {}", shippingInstruction.getId());
                throw new ValidationException("Invalid getNonNegoFreightCopies!");
            }

            if (Objects.nonNull(shippingInstruction.getNonNegoUnFreightCopies()) && (shippingInstruction.getNonNegoUnFreightCopies() > 100
                    || shippingInstruction.getNonNegoUnFreightCopies() < 0)) {
                log.info("Validation failed for getNonNegoUnFreightCopies for SI id : {}", shippingInstruction.getId());
                throw new ValidationException("Invalid getNonNegoUnFreightCopies!");
            }
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
        var commodityTypesFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> shippingInstructionMasterDataHelper.addAllCommodityTypesInSingleCall(shippingInstructionResponse, masterDataResponse)), executorServiceMasterData);
        var containerTypeFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> shippingInstructionMasterDataHelper.addAllContainerTypesInSingleCall(shippingInstructionResponse, masterDataResponse)), executorServiceMasterData);

        CompletableFuture.allOf(masterListFuture, unLocationsFuture, carrierFuture, commodityTypesFuture, containerTypeFuture, vesselsFuture).join();
        return masterDataResponse;
    }

    private void sendDataToKafka(ShippingInstruction shippingInstruction) {
        try {
            // Convert object to JSON string
            String jsonPayload = jsonHelper.convertToJson(shippingInstruction);

            // Wrap it inside your generic payload
            GenericKafkaPayload genericKafkaMsg =
                    new GenericKafkaPayload(GenericKafkaMsgType.SI, jsonPayload);

            log.debug("SI Payload sent to kafka with id {}", genericKafkaMsg.getId());
            producer.produceToKafka(genericKafkaMsg, bookingQueue, genericKafkaMsg.getId());

        } catch (Exception e) {
            throw new RuntimeException("Error serializing ShippingInstruction", e);
        }
    }
}
