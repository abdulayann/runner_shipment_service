package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.VerifiedGrossMassConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.impl.CarrierBookingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IVerifiedGrossMassDao;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.VerifiedGrossMassRequest;
import com.dpw.runner.shipment.services.dto.response.FieldClassDto;
import com.dpw.runner.shipment.services.dto.response.PartiesResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.CommonContainerResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.ReferenceNumberResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.SailingInformationResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.VerifiedGrossMassListResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.VerifiedGrossMassResponse;
import com.dpw.runner.shipment.services.entity.CarrierBooking;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.ReferenceNumbers;
import com.dpw.runner.shipment.services.entity.SailingInformation;
import com.dpw.runner.shipment.services.entity.VerifiedGrossMass;
import com.dpw.runner.shipment.services.entity.enums.EntityType;
import com.dpw.runner.shipment.services.entity.enums.VerifiedGrossMassStatus;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.helpers.VerifiedGrossMassMasterDataHelper;
import com.dpw.runner.shipment.services.service.interfaces.IVerifiedGrossMassService;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.FieldUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.jetbrains.annotations.NotNull;
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
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@Slf4j
@Service
public class VerifiedGrossMassService implements IVerifiedGrossMassService {
    private final IVerifiedGrossMassDao verifiedGrossMassDao;
    private final JsonHelper jsonHelper;
    private final CarrierBookingDao carrierBookingDao;
    private final IConsolidationDetailsDao consolidationDetailsDao;
    private final CommonUtils commonUtils;
    private final MasterDataUtils masterDataUtils;
    private final ExecutorService executorServiceMasterData;
    private final VerifiedGrossMassMasterDataHelper verifiedGrossMassMasterDataHelper;


    public VerifiedGrossMassService(IVerifiedGrossMassDao verifiedGrossMassDao, JsonHelper jsonHelper, CarrierBookingDao carrierBookingDao, IConsolidationDetailsDao consolidationDetailsDao, CommonUtils commonUtils, MasterDataUtils masterDataUtils, @Qualifier("executorServiceMasterData") ExecutorService executorServiceMasterData, VerifiedGrossMassMasterDataHelper verifiedGrossMassMasterDataHelper) {
        this.verifiedGrossMassDao = verifiedGrossMassDao;
        this.jsonHelper = jsonHelper;
        this.carrierBookingDao = carrierBookingDao;
        this.consolidationDetailsDao = consolidationDetailsDao;
        this.commonUtils = commonUtils;
        this.masterDataUtils = masterDataUtils;
        this.executorServiceMasterData = executorServiceMasterData;
        this.verifiedGrossMassMasterDataHelper = verifiedGrossMassMasterDataHelper;
    }

    @Override
    public VerifiedGrossMassResponse create(VerifiedGrossMassRequest request) {
        VerifiedGrossMass verifiedGrossMass = jsonHelper.convertValue(request, VerifiedGrossMass.class);
        VerifiedGrossMass savedEntity = verifiedGrossMassDao.save(verifiedGrossMass);
        return jsonHelper.convertValue(savedEntity, VerifiedGrossMassResponse.class);
    }

    @Override
    public VerifiedGrossMassResponse retrieveById(Long id) {
        Optional<VerifiedGrossMass> verifiedGrossMass = verifiedGrossMassDao.findById(id);
        if (verifiedGrossMass.isEmpty()) {
            throw new ValidationException("Invalid vgm id");
        }
        return jsonHelper.convertValue(verifiedGrossMass.get(), VerifiedGrossMassResponse.class);
    }

    @Override
    public ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel, boolean getMasterData) {
        ListCommonRequest listCommonRequest = (ListCommonRequest) commonRequestModel.getData();
        if (listCommonRequest == null) {
            log.error(VerifiedGrossMassConstants.VERIFIED_GROSS_MASS_LIST_REQUEST_EMPTY_ERROR, LoggerHelper.getRequestIdFromMDC());
            throw new ValidationException(VerifiedGrossMassConstants.VERIFIED_GROSS_MASS_LIST_REQUEST_NULL_ERROR);
        }
        if (listCommonRequest.getIncludeColumns() == null || listCommonRequest.getIncludeColumns().isEmpty()) {
            throw new ValidationException(VerifiedGrossMassConstants.VERIFIED_GROSS_MASS_INCLUDE_COLUMNS_REQUIRED_ERROR_MESSAGE);
        }

        Pair<Specification<VerifiedGrossMass>, Pageable> tuple = fetchData(listCommonRequest, VerifiedGrossMass.class, VerifiedGrossMassConstants.tableNames);
        Page<VerifiedGrossMass> verifiedGrossMassPage = verifiedGrossMassDao.findAll(tuple.getLeft(), tuple.getRight());
        log.info(VerifiedGrossMassConstants.VERIFIED_GROSS_MASS_LIST_RESPONSE_SUCCESS, LoggerHelper.getRequestIdFromMDC());


        List<IRunnerResponse> filteredList = convertEntityListToDtoList(verifiedGrossMassPage.getContent(), getMasterData, listCommonRequest.getIncludeColumns().stream().collect(Collectors.toSet()));

        return ResponseHelper.buildListSuccessResponse(
                filteredList,
                verifiedGrossMassPage.getTotalPages(),
                verifiedGrossMassPage.getTotalElements());
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<VerifiedGrossMass> verifiedGrossMassList, boolean getMasterData,
                                                             Set<String> includeColumns) {
        List<VerifiedGrossMassListResponse> verifiedGrossMassListResponses = new ArrayList<>();

        for (VerifiedGrossMass verifiedGrossMass : verifiedGrossMassList) {
            VerifiedGrossMassListResponse verifiedGrossMassListResponse = jsonHelper.convertValue(verifiedGrossMass, VerifiedGrossMassListResponse.class);
            verifiedGrossMassListResponses.add(verifiedGrossMassListResponse);
        }

        List<IRunnerResponse> responseList = new ArrayList<>(verifiedGrossMassListResponses);
        return responseList;
    }

    @Override
    public VerifiedGrossMassResponse update(VerifiedGrossMassRequest request) {
        if (Objects.isNull(request.getId())) {
            throw new ValidationException("Id can not be null");
        }
        Optional<VerifiedGrossMass> verifiedGrossMassOptional = verifiedGrossMassDao.findById(request.getId());
        if (verifiedGrossMassOptional.isEmpty()) {
            throw new ValidationException("Invalid verified gross mass id");
        }
        VerifiedGrossMass verifiedGrossMass = jsonHelper.convertValue(request, VerifiedGrossMass.class);
        VerifiedGrossMass savedEntity = verifiedGrossMassDao.save(verifiedGrossMass);
        return jsonHelper.convertValue(savedEntity, VerifiedGrossMassResponse.class);
    }

    @Override
    public void delete(Long id) {
        verifiedGrossMassDao.delete(id);
    }

    @Override
    public ResponseEntity<IRunnerResponse> getAllMasterData(Long vgmId) {
        String responseMsg;
        try {
            Optional<VerifiedGrossMass> verifiedGrossMassOptional = verifiedGrossMassDao.findById(vgmId);
            if (verifiedGrossMassOptional.isEmpty()) {
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            VerifiedGrossMass verifiedGrossMass = verifiedGrossMassOptional.get();
            long start = System.currentTimeMillis();
            List<String> includeColumns = FieldUtils.getMasterDataAnnotationFields(List.of(createFieldClassDto(VerifiedGrossMass.class, null), createFieldClassDto(SailingInformation.class, "sailingInformation.")));
            includeColumns.addAll(VerifiedGrossMassConstants.LIST_INCLUDE_COLUMNS);
            VerifiedGrossMassResponse verifiedGrossMassResponse = (VerifiedGrossMassResponse) commonUtils.setIncludedFieldsToResponse(verifiedGrossMass, new HashSet<>(includeColumns), new VerifiedGrossMassResponse());
            log.info("Total time taken in setting verified gross mass details response {}", (System.currentTimeMillis() - start));
            Map<String, Object> response = fetchAllMasterDataByKey(verifiedGrossMassResponse);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_DATA_RETRIEVAL_FAILURE;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private FieldClassDto createFieldClassDto(Class<?> clazz, String parentref) {
        FieldClassDto fieldClassDto = new FieldClassDto();
        fieldClassDto.setClazz(clazz);
        fieldClassDto.setFieldRef(parentref);
        return fieldClassDto;
    }

    public Map<String, Object> fetchAllMasterDataByKey(VerifiedGrossMassResponse verifiedGrossMassResponse) {
        Map<String, Object> masterDataResponse = new HashMap<>();
        var masterListFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> verifiedGrossMassMasterDataHelper.addAllMasterDataInSingleCall(verifiedGrossMassResponse, masterDataResponse)), executorServiceMasterData);
        var unLocationsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> verifiedGrossMassMasterDataHelper.addAllUnlocationDataInSingleCall(verifiedGrossMassResponse, masterDataResponse)), executorServiceMasterData);
        var carrierFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> verifiedGrossMassMasterDataHelper.addAllCarrierDataInSingleCall(verifiedGrossMassResponse, masterDataResponse)), executorServiceMasterData);
        var commodityTypesFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> verifiedGrossMassMasterDataHelper.addAllCommodityTypesInSingleCall(verifiedGrossMassResponse, masterDataResponse)), executorServiceMasterData);
        var containerTypeFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> verifiedGrossMassMasterDataHelper.addAllContainerTypesInSingleCall(verifiedGrossMassResponse, masterDataResponse)), executorServiceMasterData);
        var vesselsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> verifiedGrossMassMasterDataHelper.addAllVesselDataInSingleCall(verifiedGrossMassResponse, masterDataResponse)), executorServiceMasterData);
        CompletableFuture.allOf(masterListFuture, unLocationsFuture, carrierFuture, commodityTypesFuture, containerTypeFuture, vesselsFuture).join();

        return masterDataResponse;
    }

    @Override
    public VerifiedGrossMassResponse getDefaultVerifiedGrossMassValues(EntityType type, Long entityId) {
        VerifiedGrossMassResponse verifiedGrossMassResponse = new VerifiedGrossMassResponse();
        verifiedGrossMassResponse.setEntityType(type);
        if (EntityType.CARRIER_BOOKING.equals(type)) {
            setDefaultDataFromCarrierBooking(entityId, verifiedGrossMassResponse);

        } else if (EntityType.CONSOLIDATION.equals(type)) {
            setDefaultDataFromConsol(entityId, verifiedGrossMassResponse);
        } else {
            throw new ValidationException("Invalid value of Entity Type");
        }
        return verifiedGrossMassResponse;
    }

    private void setDefaultDataFromConsol(Long entityId, VerifiedGrossMassResponse verifiedGrossMassResponse) {
        ConsolidationDetails consolidationDetails = consolidationDetailsDao.findConsolidationsById(entityId);
        if (Objects.isNull(consolidationDetails)) {
            throw new ValidationException("Invalid consolidation id");
        }
        verifiedGrossMassResponse.setEntityId(consolidationDetails.getId());
        verifiedGrossMassResponse.setEntityNumber(consolidationDetails.getConsolidationNumber());
        SailingInformationResponse sailingInformationResponse = new SailingInformationResponse();
        sailingInformationResponse.setCarrier(consolidationDetails.getCarrierDetails().getShippingLine());

        verifiedGrossMassResponse.setSailingInformation(sailingInformationResponse);
        //sending agent is origin agent, receivingAgent is destination agent in consol
        if (Objects.nonNull(consolidationDetails.getSendingAgent())) {
            PartiesResponse partiesResponse = jsonHelper.convertValue(consolidationDetails.getSendingAgent(), PartiesResponse.class);
            partiesResponse.setId(null);
            partiesResponse.setGuid(null);
            verifiedGrossMassResponse.setRequestor(partiesResponse);
            verifiedGrossMassResponse.setResponsible(partiesResponse);
            verifiedGrossMassResponse.setAuthorised(partiesResponse);
        }
        List<ReferenceNumberResponse> referenceNumberResponses = getReferenceNumberResponses(consolidationDetails);
        verifiedGrossMassResponse.setReferenceNumbersList(referenceNumberResponses);
        List<CommonContainerResponse> commonContainersList = new ArrayList<>();
        if (!CollectionUtils.isEmpty(consolidationDetails.getContainersList())) {
            for (Containers containers : consolidationDetails.getContainersList()) {
                CommonContainerResponse commonContainers = getCommonContainerResponse(containers);
                commonContainersList.add(commonContainers);
            }
        }
        verifiedGrossMassResponse.setContainersList(commonContainersList);
    }

    @NotNull
    private static List<ReferenceNumberResponse> getReferenceNumberResponses(ConsolidationDetails consolidationDetails) {
        List<ReferenceNumberResponse> referenceNumberResponses = new ArrayList<>();
        if (!CollectionUtils.isEmpty(consolidationDetails.getReferenceNumbersList())) {
            for (ReferenceNumbers referenceNumbers : consolidationDetails.getReferenceNumbersList()) {
                ReferenceNumberResponse referenceNumberResponse = new ReferenceNumberResponse();
                referenceNumberResponse.setType(referenceNumbers.getType());
                referenceNumberResponse.setReferenceNumber(referenceNumbers.getReferenceNumber());
                referenceNumberResponses.add(referenceNumberResponse);
            }
        }
        return referenceNumberResponses;
    }

    private void setDefaultDataFromCarrierBooking(Long entityId, VerifiedGrossMassResponse verifiedGrossMassResponse) {
        Optional<CarrierBooking> carrierBookingEntity = carrierBookingDao.findById(entityId);
        if (carrierBookingEntity.isEmpty()) {
            throw new ValidationException("Invalid carrier booking id");
        }
        CarrierBooking carrierBooking = carrierBookingEntity.get();
        verifiedGrossMassResponse.setBookingStatus(carrierBooking.getStatus());
        verifiedGrossMassResponse.setStatus(VerifiedGrossMassStatus.Draft);
        if (Objects.nonNull(carrierBooking.getShippingInstruction())) {
            verifiedGrossMassResponse.setSiStatus(carrierBooking.getShippingInstruction().getStatus());
        }
        verifiedGrossMassResponse.setEntityId(carrierBooking.getId());
        verifiedGrossMassResponse.setEntityNumber(carrierBooking.getBookingNo());
        verifiedGrossMassResponse.setCarrierBlNo(carrierBooking.getCarrierBlNo());
        verifiedGrossMassResponse.setCarrierBookingNo(carrierBooking.getCarrierBookingNo());

        SailingInformationResponse sailingInformationResponse = new SailingInformationResponse();
        sailingInformationResponse.setCarrier(carrierBooking.getSailingInformation().getCarrier());

        verifiedGrossMassResponse.setSailingInformation(sailingInformationResponse);
        //reference numbers
        List<ReferenceNumberResponse> referenceNumbersResponses = new ArrayList<>();
        if (!CollectionUtils.isEmpty(carrierBooking.getReferenceNumbersList())) {
            for (ReferenceNumbers referenceNumbers : carrierBooking.getReferenceNumbersList()) {
                ReferenceNumberResponse referenceNumbersResponse = new ReferenceNumberResponse();
                referenceNumbersResponse.setReferenceNumber(referenceNumbers.getReferenceNumber());
                referenceNumbersResponse.setType(referenceNumbers.getType());
                referenceNumbersResponses.add(referenceNumbersResponse);
            }
        }
        verifiedGrossMassResponse.setReferenceNumbersList(referenceNumbersResponses);

        //containers
        if (!CollectionUtils.isEmpty(carrierBooking.getContainersList())) {
            verifiedGrossMassResponse.setContainersList(jsonHelper.convertValueToList(carrierBooking.getContainersList(), CommonContainerResponse.class));
            verifiedGrossMassResponse.getContainersList()
                    .forEach(container -> {
                        container.setId(null);
                        container.setGuid(null);
                    });
        }
        //parties
        if (Objects.nonNull(carrierBooking.getRequester())) {
            PartiesResponse partiesResponse = jsonHelper.convertValue(carrierBooking.getRequester(), PartiesResponse.class);
            partiesResponse.setId(null);
            partiesResponse.setGuid(null);
            verifiedGrossMassResponse.setRequestor(partiesResponse);
        }
        if (Objects.nonNull(carrierBooking.getShipper())) {
            PartiesResponse partiesResponse = jsonHelper.convertValue(carrierBooking.getShipper(), PartiesResponse.class);
            partiesResponse.setId(null);
            partiesResponse.setGuid(null);
            verifiedGrossMassResponse.setResponsible(partiesResponse);
        }
        if (Objects.nonNull(carrierBooking.getForwardingAgent())) {
            PartiesResponse partiesResponse = jsonHelper.convertValue(carrierBooking.getForwardingAgent(), PartiesResponse.class);
            partiesResponse.setId(null);
            partiesResponse.setGuid(null);
            verifiedGrossMassResponse.setAuthorised(partiesResponse);
        }
    }

    private static CommonContainerResponse getCommonContainerResponse(Containers containers) {
        CommonContainerResponse commonContainers = new CommonContainerResponse();
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
        return commonContainers;
    }
}

