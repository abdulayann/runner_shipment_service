package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.CarrierBookingConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.ICarrierBookingDao;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.CarrierBookingRequest;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.CarrierBookingListResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.CarrierBookingResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.CarrierBooking;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.SailingInformation;
import com.dpw.runner.shipment.services.entity.enums.CarrierBookingGenerationType;
import com.dpw.runner.shipment.services.entity.enums.CarrierBookingStatus;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.CarrierBookingMasterDataHelper;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.ICarrierBookingService;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.dpw.runner.shipment.services.utils.v3.CarrierBookingValidationUtil;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.commons.constants.CarrierBookingConstants.CARRIER_INCLUDE_COLUMNS_REQUIRED_ERROR_MESSAGE;
import static com.dpw.runner.shipment.services.commons.constants.CarrierBookingConstants.CARRIER_LIST_REQUEST_EMPTY_ERROR;
import static com.dpw.runner.shipment.services.commons.constants.CarrierBookingConstants.CARRIER_LIST_REQUEST_NULL_ERROR;
import static com.dpw.runner.shipment.services.commons.constants.CarrierBookingConstants.CARRIER_LIST_RESPONSE_SUCCESS;
import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@Slf4j
@Service
public class CarrierBookingService implements ICarrierBookingService {

    private final ICarrierBookingDao carrierBookingDao;
    private final JsonHelper jsonHelper;
    private final CarrierBookingMasterDataHelper carrierBookingMasterDataHelper;
    private final CarrierBookingValidationUtil carrierBookingValidationUtil;
    private final CommonUtils commonUtils;

    @Autowired
    public CarrierBookingService(ICarrierBookingDao carrierBookingDao, JsonHelper jsonHelper, CarrierBookingMasterDataHelper carrierBookingMasterDataHelper, CarrierBookingValidationUtil carrierBookingValidationUtil, CommonUtils commonUtils) {
        this.carrierBookingDao = carrierBookingDao;
        this.jsonHelper = jsonHelper;
        this.carrierBookingMasterDataHelper = carrierBookingMasterDataHelper;
        this.carrierBookingValidationUtil = carrierBookingValidationUtil;
        this.commonUtils = commonUtils;
    }

    @Override
    public CarrierBookingResponse create(CarrierBookingRequest request) {
        log.info("CarrierBookingService.create() called with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        carrierBookingValidationUtil.validateServiceType(request);
        Object entity = carrierBookingValidationUtil.validateRequest(request);
        CarrierBooking carrierBookingEntity = jsonHelper.convertValue(request, CarrierBooking.class);
        if (Constants.CONSOLIDATION.equalsIgnoreCase(request.getEntityType())) {
            ConsolidationDetails consolidationDetails = (ConsolidationDetails) entity;
            carrierBookingEntity.setEntityNumber(consolidationDetails.getConsolidationNumber());
            //read only fields
            SailingInformation sailingInformation = carrierBookingEntity.getSailingInformation();
            sailingInformation.setPol(consolidationDetails.getCarrierDetails().getOriginPort());
            sailingInformation.setPod(consolidationDetails.getCarrierDetails().getDestinationPort());
            sailingInformation.setCarrierReceiptPlace(consolidationDetails.getCarrierDetails().getOrigin());
            sailingInformation.setCarrierDeliveryPlace(consolidationDetails.getCarrierDetails().getDestination());
        }
        carrierBookingEntity.setCarrierRoutingList(null);// we will get it from carrier
        carrierBookingEntity.setLoadedContainerDropOffDetails(null); // we will get it from carrier
        carrierBookingEntity.setEmptyContainerPickupDetails(null); // we will get it from carrier
        carrierBookingEntity.setCarrierComment(null); //we will get it from carrier
        generateBookingNumber(carrierBookingEntity);
        carrierBookingEntity.setCarrierBlNo(null);
        carrierBookingEntity.setCarrierBookingNo(null);
        carrierBookingEntity.setStatus(CarrierBookingStatus.Draft);
        CarrierBooking savedEntity = carrierBookingDao.create(carrierBookingEntity);
        CarrierBookingResponse carrierBookingResponse = jsonHelper.convertValue(savedEntity, CarrierBookingResponse.class);
        log.info("CarrierBookingService.create() successful with RequestId: {} and response: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(carrierBookingResponse));
        return carrierBookingResponse;
    }

    private void generateBookingNumber(CarrierBooking carrierBookingEntity) {
        V1TenantSettingsResponse v1TenantSettingsResponse = commonUtils.getCurrentTenantSettings();
        var prefix = "";
        if (StringUtility.isNotEmpty(v1TenantSettingsResponse.getBookingPrefix())) {
            prefix = v1TenantSettingsResponse.getBookingPrefix();
        }
        if (v1TenantSettingsResponse.getBookingNumberGeneration() == CarrierBookingGenerationType.Serial.getValue()) {
            //get total count of carrier booking for that tenant + 1, prefix+count
            Long totalCarrierBookings = carrierBookingDao.getTotalCarrierBookings();
            carrierBookingEntity.setBookingNo(prefix + totalCarrierBookings + 1);
        } else {
            //generate the random number and add prefix
            String randomBookingNumber = StringUtility.getRandomString(10);
            carrierBookingEntity.setBookingNo(prefix + randomBookingNumber);
        }

    }

    @Override
    public CarrierBookingResponse findById(Long id) {
        log.info("CarrierBookingService.getById() called with RequestId: {} and id: {}",
                LoggerHelper.getRequestIdFromMDC(), id);
        CarrierBooking carrierBooking = carrierBookingDao.findById(id).orElseThrow(() -> new ValidationException("Invalid id : " + id));
        CarrierBookingResponse carrierBookingResponse = jsonHelper.convertValue(carrierBooking, CarrierBookingResponse.class);
        log.info("CarrierBookingService.getById() successful with RequestId: {} and response: {}",
                LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(carrierBookingResponse));
        return carrierBookingResponse;
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

        Pair<Specification<CarrierBooking>, Pageable> tuple = fetchData(listCommonRequest, CarrierBooking.class, CarrierBookingConstants.tableNames);
        Page<CarrierBooking> carrierBookingPage = carrierBookingDao.findAll(tuple.getLeft(), tuple.getRight());
        log.info(CARRIER_LIST_RESPONSE_SUCCESS, LoggerHelper.getRequestIdFromMDC());


        List<IRunnerResponse> filteredList = convertEntityListToDtoList(carrierBookingPage.getContent(), getMasterData, listCommonRequest.getIncludeColumns().stream().collect(Collectors.toSet()));

        return ResponseHelper.buildListSuccessResponse(
                filteredList,
                carrierBookingPage.getTotalPages(),
                carrierBookingPage.getTotalElements());
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<CarrierBooking> carrierBookingList, boolean getMasterData,
                                                             Set<String> includeColumns) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        List<CarrierBookingListResponse> carrierBookingListResponses = new ArrayList<>();

        for (CarrierBooking carrierBooking : carrierBookingList) {
            CarrierBookingListResponse carrierBookingListResponse = jsonHelper.convertValue(carrierBooking, CarrierBookingListResponse.class);
            carrierBookingListResponses.add(carrierBookingListResponse);
        }

        carrierBookingListResponses.forEach(responseList::add);
        carrierBookingMasterDataHelper.getMasterDataForList(carrierBookingList, responseList, getMasterData, true, includeColumns);
        return responseList;
    }

    @Override
    public CarrierBookingResponse update(CarrierBookingRequest request) {
        log.info("CarrierBookingService.update() called with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        CarrierBooking existingCarrierBooking = carrierBookingDao.findById(request.getId()).orElseThrow(() -> new ValidationException("Invalid carrier booking Id"));
        carrierBookingValidationUtil.validateServiceType(request);
        Object entity = carrierBookingValidationUtil.validateRequest(request);
        CarrierBooking carrierBookingEntity = jsonHelper.convertValue(request, CarrierBooking.class);
        if (Constants.CONSOLIDATION.equalsIgnoreCase(request.getEntityType())) {
            ConsolidationDetails consolidationDetails = (ConsolidationDetails) entity;
            carrierBookingEntity.setEntityNumber(consolidationDetails.getConsolidationNumber());
            //read only fields
            SailingInformation sailingInformation = carrierBookingEntity.getSailingInformation();
            if (sailingInformation == null) {
                sailingInformation = new SailingInformation();
            }
            sailingInformation.setPol(consolidationDetails.getCarrierDetails().getOriginPort());
            sailingInformation.setPod(consolidationDetails.getCarrierDetails().getDestinationPort());
            sailingInformation.setCarrierReceiptPlace(consolidationDetails.getCarrierDetails().getOrigin());
            sailingInformation.setCarrierDeliveryPlace(consolidationDetails.getCarrierDetails().getDestination());
            carrierBookingEntity.setSailingInformation(sailingInformation);
        }
        carrierBookingEntity.setCarrierRoutingList(existingCarrierBooking.getCarrierRoutingList());// we will get it from carrier
        carrierBookingEntity.setLoadedContainerDropOffDetails(existingCarrierBooking.getLoadedContainerDropOffDetails()); // we will get it from carrier
        carrierBookingEntity.setEmptyContainerPickupDetails(existingCarrierBooking.getEmptyContainerPickupDetails()); // we will get it from carrier
        carrierBookingEntity.setCarrierComment(existingCarrierBooking.getCarrierComment()); //we will get it from carrier
        carrierBookingEntity.setBookingNo(existingCarrierBooking.getBookingNo());
        carrierBookingEntity.setCarrierBlNo(existingCarrierBooking.getCarrierBlNo());
        carrierBookingEntity.setCarrierBookingNo(existingCarrierBooking.getCarrierBookingNo());
        if (!(CarrierBookingStatus.Draft.equals(carrierBookingEntity.getStatus()) || CarrierBookingStatus.ChangeDraft.equals(carrierBookingEntity.getStatus()))) {
            carrierBookingEntity.setStatus(existingCarrierBooking.getStatus());
        }
        CarrierBooking savedEntity = carrierBookingDao.create(carrierBookingEntity);
        CarrierBookingResponse carrierBookingResponse = jsonHelper.convertValue(savedEntity, CarrierBookingResponse.class);
        log.info("CarrierBookingService.update() successful with RequestId: {} and response: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(carrierBookingResponse));
        return carrierBookingResponse;
    }

    @Override
    public void delete(Long id) {
        log.info("CarrierBookingService.delete() called with RequestId: {} and id: {}",
                LoggerHelper.getRequestIdFromMDC(), id);
        carrierBookingDao.delete(id);
        log.info("CarrierBookingService.delete() successful with RequestId: {} and id: {}",
                LoggerHelper.getRequestIdFromMDC(), id);
    }
}

