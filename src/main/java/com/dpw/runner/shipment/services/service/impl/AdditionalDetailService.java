//package com.dpw.runner.shipment.services.service.impl;
//
//import com.dpw.runner.shipment.services.commons.constants.AdditionalDetailConstants;
//import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
//import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
//import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
//import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
//import com.dpw.runner.shipment.services.dao.interfaces.IAdditionalDetailDao;
//import com.dpw.runner.shipment.services.dto.request.AdditionalDetailRequest;
//import com.dpw.runner.shipment.services.dto.response.AdditionalDetailResponse;
//import com.dpw.runner.shipment.services.entity.AdditionalDetails;
//import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
//import com.dpw.runner.shipment.services.helpers.JsonHelper;
//import com.dpw.runner.shipment.services.helpers.LoggerHelper;
//import com.dpw.runner.shipment.services.helpers.ResponseHelper;
//import com.dpw.runner.shipment.services.service.interfaces.IAdditionalDetailService;
//import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
//import lombok.extern.slf4j.Slf4j;
//import org.springframework.beans.factory.annotation.Autowired;
//import org.springframework.dao.DataRetrievalFailureException;
//import org.springframework.http.ResponseEntity;
//import org.springframework.scheduling.annotation.Async;
//import org.springframework.stereotype.Service;
//import org.springframework.transaction.annotation.Transactional;
//
//import java.util.ArrayList;
//import java.util.List;
//import java.util.Optional;
//import java.util.concurrent.CompletableFuture;
//
//@SuppressWarnings("ALL")
//@Service
//@Slf4j
//public class AdditionalDetailService implements IAdditionalDetailService {
//
//    @Autowired
//    private IAdditionalDetailDao additionalDetailDao;
//
//    @Autowired
//    private JsonHelper jsonHelper;
//
//    @Autowired
//    private IAuditLogService auditLogService;
//
//    @Transactional
//    @Override
//    public ResponseEntity<IRunnerResponse> create(CommonRequestModel commonRequestModel) {
//        return null;
//    }
//
//    @Transactional
//    @Override
//    public ResponseEntity<IRunnerResponse> update(CommonRequestModel commonRequestModel) throws RunnerException {
//        return null;
//    }
//
//    @Override
//    public ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel) {
//        return null;
//    }
//
//    @Override
//    @Async
//    public CompletableFuture<ResponseEntity<IRunnerResponse>> listAsync(CommonRequestModel commonRequestModel) {
//        return null;
//    }
//
//    @Override
//    public ResponseEntity<IRunnerResponse> delete(CommonRequestModel commonRequestModel) {
//        return null;
//    }
//
//    @Override
//    public ResponseEntity<IRunnerResponse> retrieveById(CommonRequestModel commonRequestModel) {
//        String responseMsg;
//        try {
//            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
//            if (request == null) {
//                log.debug("Request is empty for additional details retrieve for Request Id {}", LoggerHelper.getRequestIdFromMDC());
//            }
//            if (request.getId() == null) {
//                log.debug("Request Id is null for additional details retrieve for Request Id {}", LoggerHelper.getRequestIdFromMDC());
//            }
//            long id = request.getId();
//            Optional<AdditionalDetails> additionalDetails = additionalDetailDao.findById(id);
//            if (!additionalDetails.isPresent()) {
//                log.debug(AdditionalDetailConstants.ADDITIONAL_DETAILS_RETRIEVE_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
//                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
//            }
//            log.info("Additional details fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
//            AdditionalDetailResponse response = convertEntityToDto(additionalDetails.get());
//            return ResponseHelper.buildSuccessResponse(response);
//        } catch (Exception e) {
//            responseMsg = e.getMessage() != null ? e.getMessage()
//                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
//            log.error(responseMsg, e);
//            return ResponseHelper.buildFailedResponse(responseMsg);
//        }
//    }
//
//    private AdditionalDetailResponse convertEntityToDto(AdditionalDetails additionalDetails) {
//        return jsonHelper.convertValue(additionalDetails, AdditionalDetailResponse.class);
//    }
//
//    private AdditionalDetails convertRequestToEntity(AdditionalDetailRequest request) {
//        return jsonHelper.convertValue(request, AdditionalDetails.class);
//    }
//
//    private List<IRunnerResponse> convertEntityListToDtoList(List<AdditionalDetails> list) {
//        List<IRunnerResponse> responseList = new ArrayList<>();
//        list.forEach(additionalDetails -> responseList.add(convertEntityToDto(additionalDetails)));
//        return responseList;
//    }
//}
