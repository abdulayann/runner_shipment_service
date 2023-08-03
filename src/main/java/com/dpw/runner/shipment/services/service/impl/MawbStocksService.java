package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IMawbStocksDao;
import com.dpw.runner.shipment.services.dao.interfaces.IMawbStocksLinkDao;
import com.dpw.runner.shipment.services.dto.request.MawbStocksRequest;
import com.dpw.runner.shipment.services.dto.response.MawbStocksResponse;
import com.dpw.runner.shipment.services.entity.MawbStocks;
import com.dpw.runner.shipment.services.entity.MawbStocksLink;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IMawbStocksService;
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
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@SuppressWarnings("ALL")
@Slf4j
@Service
public class MawbStocksService implements IMawbStocksService {
    @Autowired
    private IMawbStocksDao mawbStocksDao;

    @Autowired
    private IMawbStocksLinkDao mawbStocksLinkDao;

    @Autowired
    private ModelMapper modelMapper;

    @Transactional
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) {
        String responseMsg;
        MawbStocksRequest request = null;
        request = (MawbStocksRequest) commonRequestModel.getData();
        if (request == null) {
            log.debug("Request is empty for MAWB stocks create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        MawbStocks mawbStocks = convertRequestToEntity(request);

        try {
           mawbStocks = mawbStocksDao.save(mawbStocks);
            log.info("MAWB stocks created successfully for Id {} with Request Id {}", mawbStocks.getId(), LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(mawbStocks));
    }

    @Transactional
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) {
        String responseMsg;
        MawbStocksRequest request = (MawbStocksRequest) commonRequestModel.getData();
        if (request == null) {
            log.debug("Request is empty for MAWB Stocks update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

        if (request.getId() == null) {
            log.debug("Request Id is null for MAWB Stocks update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        long id = request.getId();
        Optional<MawbStocks> oldEntity = mawbStocksDao.findById(id);
        if (!oldEntity.isPresent()) {
            log.debug("MAWB Stocks is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        MawbStocks mawbStocks = convertRequestToEntity(request);
        try {
            mawbStocks = mawbStocksDao.save(mawbStocks);
            log.info("Updated the MAWB stocks for Id {} with Requestr Id {}", id, LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(mawbStocks));
    }

    public ResponseEntity<?> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for MAWB stocks list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            // construct specifications for filter request
            Pair<Specification<MawbStocks>, Pageable> tuple = fetchData(request, MawbStocks.class);
            Page<MawbStocks> mawbStocksPage = mawbStocksDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("MAWB stocks list retrieved successfully for RequestId {} ", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(mawbStocksPage.getContent()),
                    mawbStocksPage.getTotalPages(),
                    mawbStocksPage.getTotalElements());
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
            if (request == null) {
                log.error("Request is empty for MAWB stocks async list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            // construct specifications for filter request
            Pair<Specification<MawbStocks>, Pageable> tuple = fetchData(request, MawbStocks.class);
            Page<MawbStocks> mawbStocksPage = mawbStocksDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("MAWB stocks async list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return CompletableFuture.completedFuture(ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(mawbStocksPage.getContent()),
                    mawbStocksPage.getTotalPages(),
                    mawbStocksPage.getTotalElements()));
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
            if (request == null) {
                log.debug("Request is empty for MAWB stocks delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.debug("Request Id is null for MAWB Stocks delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();

            Optional<MawbStocks> mawbStocks = mawbStocksDao.findById(id);
            if (!mawbStocks.isPresent()) {
                log.debug("MAWB Stocks is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            mawbStocksDao.delete(mawbStocks.get());
            log.info("Deleted MAWB Stocks for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
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
            if (request == null) {
                log.error("Request is empty for MAWB Stocks retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.error("Request Id is null for MAWB Stocks retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<MawbStocks> mawbStocks = mawbStocksDao.findById(id);
            if (!mawbStocks.isPresent()) {
                log.debug("MAWB Stocks is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("MAWB Stocks details fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            MawbStocksResponse response = convertEntityToDto(mawbStocks.get());
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private MawbStocksResponse convertEntityToDto(MawbStocks mawbStocks) {
        return modelMapper.map(mawbStocks, MawbStocksResponse.class);
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<MawbStocks> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(mawbStocks -> {
            responseList.add(convertEntityToDto(mawbStocks));
        });
        return responseList;
    }

    private MawbStocks convertRequestToEntity(MawbStocksRequest request) {
        return modelMapper.map(request, MawbStocks.class);
    }

    private void setManyToOneRelationships(MawbStocks mawbStocks){
        if(mawbStocks.getMawbStocksLinkRows() !=null){
            List<MawbStocksLink> mawbStocksLinks = mawbStocks.getMawbStocksLinkRows();
            for (MawbStocksLink mawbStocksLink: mawbStocksLinks) {
                mawbStocksLink.setMawbStocks(mawbStocks);
            }
        }
    }
}
