package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IDefaultViewsDao;
import com.dpw.runner.shipment.services.dto.request.DefaultViewsRequest;
import com.dpw.runner.shipment.services.dto.response.DefaultViewsResponse;
import com.dpw.runner.shipment.services.entity.DefaultViews;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IDefaultViewsService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;

@SuppressWarnings("ALL")
@Service
@Slf4j
public class DefaultViewsService implements IDefaultViewsService {
    @Autowired
    private IDefaultViewsDao defaultViewsDao;

    @Autowired
    private JsonHelper jsonHelper;

    @Transactional
    public ResponseEntity<IRunnerResponse> create(CommonRequestModel commonRequestModel) {
        String responseMsg;
        DefaultViewsRequest request = null;
        request = (DefaultViewsRequest) commonRequestModel.getData();
        DefaultViews defaultView = convertRequestToEntity(request);
        try {
            defaultView = defaultViewsDao.save(defaultView);
            log.info("Default views created successfully for Id {} with Request Id {}", defaultView.getId(), LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(defaultView));
    }

    @Transactional
    public ResponseEntity<IRunnerResponse> update(CommonRequestModel commonRequestModel) {
        String responseMsg;
        DefaultViewsRequest request = (DefaultViewsRequest) commonRequestModel.getData();

        if (request.getId() == null) {
            log.debug("Request Id is null for default views update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        Long id = request.getId();
        Optional<DefaultViews> oldEntity = defaultViewsDao.findById(id);
        if (!oldEntity.isPresent()) {
            log.debug(Constants.DEFAULT_VIEW_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        DefaultViews view = convertRequestToEntity(request);
        view.setId(oldEntity.get().getId());
        try {
            view = defaultViewsDao.save(view);
            log.info("Updated the views for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(view));
    }

    public ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            List<DefaultViews> viewsList = defaultViewsDao.findAll();
            log.info("Default views list retrieved successfully with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildListSuccessResponse(convertEntityListToDtoList(viewsList), request.getPageNo(), viewsList.size());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    @Async
    public CompletableFuture<ResponseEntity<IRunnerResponse>> listAsync(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            List<DefaultViews> viewsList = defaultViewsDao.findAll();
            log.info("Default views async list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return CompletableFuture.completedFuture(ResponseHelper.buildListSuccessResponse(convertEntityListToDtoList(viewsList), request.getPageNo(), viewsList.size()));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return CompletableFuture.completedFuture(ResponseHelper.buildFailedResponse(responseMsg));
        }
    }

    public ResponseEntity<IRunnerResponse> delete(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request.getId() == null) {
                log.debug("Request Id is null for default views delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            Long id = request.getId();

            Optional<DefaultViews> view = defaultViewsDao.findById(id);
            if (!view.isPresent()) {
                log.debug(Constants.DEFAULT_VIEW_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            defaultViewsDao.delete(view.get());
            log.info("Deleted default view for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildSuccessResponse();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public ResponseEntity<IRunnerResponse> retrieveById(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request.getId() == null) {
                log.error("Request Id is null for default view retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<DefaultViews> view = defaultViewsDao.findById(id);
            if (!view.isPresent()) {
                log.debug(Constants.DEFAULT_VIEW_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("Default view fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            DefaultViewsResponse response = convertEntityToDto(view.get());
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private DefaultViewsResponse convertEntityToDto(DefaultViews view) {
        return jsonHelper.convertValue(view, DefaultViewsResponse.class);
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<DefaultViews> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(view -> {
            responseList.add(convertEntityToDto(view));
        });
        return responseList;
    }

    public DefaultViews convertRequestToEntity(DefaultViewsRequest request) {
        return jsonHelper.convertValue(request, DefaultViews.class);
    }
}
