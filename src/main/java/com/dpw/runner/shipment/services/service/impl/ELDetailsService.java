package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IELDetailsDao;
import com.dpw.runner.shipment.services.dto.request.ELDetailsRequest;
import com.dpw.runner.shipment.services.dto.request.ElNumbersRequest;
import com.dpw.runner.shipment.services.dto.response.ELDetailsResponse;
import com.dpw.runner.shipment.services.entity.ELDetails;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IELDetailsService;
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

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;

@Service
@Slf4j
public class ELDetailsService implements IELDetailsService {
    @Autowired
    private IELDetailsDao elDetailsDao;

    @Autowired
    private JsonHelper jsonHelper;

    @Transactional
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) {
        String responseMsg;
        ELDetailsRequest request = (ELDetailsRequest) commonRequestModel.getData();
        if(request == null) {
            log.debug("Request is empty for EL Details create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        ELDetails elDetails = convertRequestToELDetails(request);
        try {
            elDetails = elDetailsDao.save(elDetails);
            log.info("EL Details created successfully for Id {} with Request Id {}", elDetails.getId(), LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(elDetails));
    }

    @Transactional
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) {
        String responseMsg;
        ELDetailsRequest request = (ELDetailsRequest) commonRequestModel.getData();
        if(request == null) {
            log.debug("Request is empty for EL details update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

        if(request.getId() == null) {
            log.debug("Request Id is null for EL details update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        long id = request.getId();
        Optional<ELDetails> oldEntity = elDetailsDao.findById(id);
        if (oldEntity.isEmpty()) {
            log.debug("EL Details is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        ELDetails elDetails = convertRequestToELDetails(request);
        elDetails.setId(oldEntity.get().getId());
        try {
            elDetails = elDetailsDao.save(elDetails);
            log.info("Updated the EL details for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(elDetails));
    }

    @Override
    public ResponseEntity<?> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if(request == null) {
                log.error("Request is empty for EL details list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            Pair<Specification<ELDetails>, Pageable> tuple = fetchData(request, ELDetails.class);
            Page<ELDetails> elDetailsPage = elDetailsDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("EL details list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(elDetailsPage.getContent()),
                    elDetailsPage.getTotalPages(),
                    elDetailsPage.getTotalElements());
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
                log.error("Request is empty for EL details async list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            Pair<Specification<ELDetails>, Pageable> tuple = fetchData(request, ELDetails.class);
            Page<ELDetails> elDetailsPage = elDetailsDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("EL details async list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return CompletableFuture.completedFuture( ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(elDetailsPage.getContent()),
                    elDetailsPage.getTotalPages(),
                    elDetailsPage.getTotalElements()));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return CompletableFuture.completedFuture(ResponseHelper.buildFailedResponse(responseMsg));
        }

    }

    @Override
    public ResponseEntity<?> delete(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if(request == null) {
                log.debug("Request is empty for EL details delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if(request.getId() == null) {
                log.debug("Request Id is null for EL details delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();

            Optional<ELDetails> elDetails = elDetailsDao.findById(id);
            if (elDetails.isEmpty()) {
                log.debug("El Details is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            elDetailsDao.delete(elDetails.get());
            log.info("Deleted EL detail for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildSuccessResponse();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<?> validateElNumber(CommonRequestModel requestModel) {
        String responseMsg;
        try {
            ElNumbersRequest request = (ElNumbersRequest) (requestModel.getData());
            if(request == null) {
                log.debug("Request is empty for EL details validate number request with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if(request.getElNumber() == null) {
                log.debug("EL number is empty for EL details validate number request with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            String elNumber = request.getElNumber();
            Optional<ELDetails> elDetails = elDetailsDao.findByElNumber(elNumber);
            if (elDetails.isEmpty()) {
                log.debug("El Detail is null for El Number {} with Request Id {}", request.getElNumber(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("EL details exist for EL number {} with Request Id {}", elNumber, LoggerHelper.getRequestIdFromMDC());
            ELDetailsResponse response = convertEntityToDto(elDetails.get());
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<?> retrieveById(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if(request == null) {
                log.error("Request is empty for EL details retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if(request.getId() == null) {
                log.error("Request Id is null for EL details retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<ELDetails> elDetail = elDetailsDao.findById(id);
            if (elDetail.isEmpty()) {
                log.debug("El Detail is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("EL details fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            ELDetailsResponse response = convertEntityToDto(elDetail.get());
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private ELDetailsResponse convertEntityToDto(ELDetails elDetails) {
        return jsonHelper.convertValue(elDetails, ELDetailsResponse.class);
    }

    private ELDetails convertRequestToELDetails(ELDetailsRequest request) {
        return jsonHelper.convertValue(request, ELDetails.class);
    }

    private List<IRunnerResponse> convertEntityListToDtoList(final List<ELDetails> lst) {
        return lst.stream()
                .map(item -> convertEntityToDto(item))
                .collect(Collectors.toList());
    }
}
