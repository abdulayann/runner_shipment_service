package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IReferenceNumbersDao;
import com.dpw.runner.shipment.services.dto.request.ReferenceNumbersRequest;
import com.dpw.runner.shipment.services.dto.response.ReferenceNumbersResponse;
import com.dpw.runner.shipment.services.entity.ReferenceNumbers;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IReferenceNumbersService;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
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

import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;

@SuppressWarnings("ALL")
@Service
@Slf4j
public class ReferenceNumbersService implements IReferenceNumbersService {
    @Autowired
    private IReferenceNumbersDao referenceNumbersDao;

    @Autowired
    private ModelMapper modelMapper;

    @Transactional
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) {
        String responseMsg;
        ReferenceNumbersRequest request = null;
        request = (ReferenceNumbersRequest) commonRequestModel.getData();
        if(request == null) {
            log.debug("Request is empty for Reference Number create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        ReferenceNumbers referenceNumbers = convertRequestToEntity(request);
        try {
            referenceNumbers = referenceNumbersDao.save(referenceNumbers);
            log.info("Reference Number Details created successfully for Id {} with Request Id {}", referenceNumbers.getId(), LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(referenceNumbers));
    }

    @Transactional
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) {
        String responseMsg;
        ReferenceNumbersRequest request = (ReferenceNumbersRequest) commonRequestModel.getData();
        if(request == null) {
            log.debug("Request is empty for Reference Number update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

        if(request.getId() == null) {
            log.debug("Request Id is null for Reference Number update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        long id = request.getId();
        Optional<ReferenceNumbers> oldEntity = referenceNumbersDao.findById(id);
        if(!oldEntity.isPresent()) {
            log.debug("Refernece Numbers is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        ReferenceNumbers referenceNumbers = convertRequestToEntity(request);
        referenceNumbers.setId(oldEntity.get().getId());
        try {
            referenceNumbers = referenceNumbersDao.save(referenceNumbers);
            log.info("Updated the Reference Number details for Id {} with Requestr Id {}", id, LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(referenceNumbers));
    }

    public ResponseEntity<?> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if(request == null) {
                log.error("Request is empty for Reference Number list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            // construct specifications for filter request
            Pair<Specification<ReferenceNumbers>, Pageable> tuple = fetchData(request, ReferenceNumbers.class);
            Page<ReferenceNumbers> referenceNumbersPage  = referenceNumbersDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Reference Number list retrieved successfully for RequestId {} ", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(referenceNumbersPage.getContent()),
                    referenceNumbersPage.getTotalPages(),
                    referenceNumbersPage.getTotalElements());
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
            if(request == null) {
                log.error("Request is empty for Reference Number async list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            // construct specifications for filter request
            Pair<Specification<ReferenceNumbers>, Pageable> tuple = fetchData(request, ReferenceNumbers.class);
            Page<ReferenceNumbers> referenceNumbersPage  = referenceNumbersDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Reference Number async list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return CompletableFuture.completedFuture( ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(referenceNumbersPage.getContent()),
                    referenceNumbersPage.getTotalPages(),
                    referenceNumbersPage.getTotalElements()));
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
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if(request == null) {
                log.debug("Request is empty for Reference Number delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if(request.getId() == null) {
                log.debug("Request Id is null for Reference Number delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();

            Optional<ReferenceNumbers> referenceNumbers = referenceNumbersDao.findById(id);
            if(!referenceNumbers.isPresent()) {
                log.debug("Reference Numbers is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            referenceNumbersDao.delete(referenceNumbers.get());
            log.info("Deleted reference number for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
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
            if(request == null) {
                log.error("Request is empty for Reference Number retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if(request.getId() == null) {
                log.error("Request Id is null for Reference Number retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<ReferenceNumbers> referenceNumbers = referenceNumbersDao.findById(id);
            if(!referenceNumbers.isPresent()) {
                log.debug("Reference Numbers is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("Reference Number details fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            ReferenceNumbersResponse response = convertEntityToDto(referenceNumbers.get());
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public ResponseEntity<?> updateEntityFromShipment(CommonRequestModel commonRequestModel, Long shipmentId) {
        String responseMsg;
        List<ReferenceNumbers> responseReferenceNumbers = new ArrayList<>();
        try {
            // TODO- Handle Transactions here
            ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentId", shipmentId, "=");
            Pair<Specification<ReferenceNumbers>, Pageable> pair = fetchData(listCommonRequest, ReferenceNumbers.class);
            Page<ReferenceNumbers> referenceNumbers = referenceNumbersDao.findAll(pair.getLeft(), pair.getRight());
            HashSet<Long> existingIds = new HashSet<>(referenceNumbers
                    .stream()
                    .map(ReferenceNumbers::getId)
                    .collect(Collectors.toList()));
            List<ReferenceNumbersRequest> referenceNumberRequestList = new ArrayList<>();
            List<ReferenceNumbersRequest> requestList = (List<ReferenceNumbersRequest>) commonRequestModel.getDataList();
            if (requestList != null && requestList.size() != 0) {
                for (ReferenceNumbersRequest request : requestList) {
                    Long id = request.getId();
                    if (id != null) {
                        existingIds.remove(id);
                    }
                    referenceNumberRequestList.add(request);
                }
                responseReferenceNumbers = saveReferenceNumbers(referenceNumberRequestList);
            }
            deleteReferenceNumbers(existingIds);
            return ResponseHelper.buildListSuccessResponse(convertEntityListToDtoList(responseReferenceNumbers));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private List<ReferenceNumbers> saveReferenceNumbers(List<ReferenceNumbersRequest> referenceNumbers) {
        List<ReferenceNumbers> res = new ArrayList<>();
        for(ReferenceNumbersRequest req : referenceNumbers){
            ReferenceNumbers saveEntity = convertRequestToEntity(req);
            if(req.getId() != null){
                long id = req.getId();
                Optional<ReferenceNumbers> oldEntity = referenceNumbersDao.findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("Reference Number is null for Id {}", req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
                saveEntity = oldEntity.get();
            }
            saveEntity = referenceNumbersDao.save(saveEntity);
            res.add(saveEntity);
        }
        return res;
    }

    private ResponseEntity<?> deleteReferenceNumbers(HashSet<Long> existingIds) {
        String responseMsg;
        try {
            for (Long id : existingIds) {
                delete(CommonRequestModel.buildRequest(CommonGetRequest.builder().id(id).build()));
            }
            return ResponseHelper.buildSuccessResponse();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private ReferenceNumbersResponse convertEntityToDto(ReferenceNumbers referenceNumbers) {
        return modelMapper.map(referenceNumbers, ReferenceNumbersResponse.class);
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<ReferenceNumbers> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(referenceNumbers -> {
            responseList.add(convertEntityToDto(referenceNumbers));
        });
        return responseList;
    }

    public ReferenceNumbers convertRequestToEntity(ReferenceNumbersRequest request) {
        return modelMapper.map(request, ReferenceNumbers.class);
    }
}
