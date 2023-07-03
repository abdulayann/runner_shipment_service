package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.DefaultViewsRequest;
import com.dpw.runner.shipment.services.dto.response.DefaultViewsResponse;
import com.dpw.runner.shipment.services.entity.DefaultViews;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IDefaultViewsDao;
import com.dpw.runner.shipment.services.service.interfaces.IDefaultViewsService;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

@SuppressWarnings("ALL")
@Service
@Slf4j
public class DefaultViewsService implements IDefaultViewsService {
    @Autowired
    private IDefaultViewsDao defaultViewsDao;

    @Autowired
    private ModelMapper modelMapper;

    @Transactional
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) {
        DefaultViewsRequest request = null;
        request = (DefaultViewsRequest) commonRequestModel.getData();
        DefaultViews defaultView = convertRequestToEntity(request);
        defaultView = defaultViewsDao.save(defaultView);
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(defaultView));
    }

    @Transactional
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) {
        DefaultViewsRequest request = (DefaultViewsRequest) commonRequestModel.getData();
        long id =request.getId();
        Optional<DefaultViews> oldEntity = defaultViewsDao.findById(id);
        if(!oldEntity.isPresent()) {
            log.debug("View is null for Id {}", request.getId());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        DefaultViews view = convertRequestToEntity(request);
        view.setId(oldEntity.get().getId());
        view = defaultViewsDao.save(view);
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(view));
    }

    public ResponseEntity<?> list(CommonRequestModel commonRequestModel){
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            List<DefaultViews> viewsList = defaultViewsDao.findAll();

            return ResponseHelper.buildListSuccessResponse(convertEntityListToDtoList(viewsList), request.getPageNo(), viewsList.size());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }

    }

    public ResponseEntity<?> delete(CommonRequestModel commonRequestModel){
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            long id =request.getId();
            Optional<DefaultViews> view = defaultViewsDao.findById(id);
            if(!view.isPresent()) {
                log.debug("View is null for Id {}", request.getId());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            defaultViewsDao.delete(view.get());
            return ResponseHelper.buildSuccessResponse();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public ResponseEntity<?> retrieveById(CommonRequestModel commonRequestModel){
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            long id =request.getId();
            Optional<DefaultViews> view = defaultViewsDao.findById(id);
            if(!view.isPresent()) {
                log.debug("View is null for Id {}", request.getId());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }

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
        DefaultViewsResponse response = new DefaultViewsResponse();
        modelMapper.map(view, response);
        return response;
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<DefaultViews> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(view -> {
            responseList.add(convertEntityToDto(view));
        });
        return responseList;
    }

    public DefaultViews convertRequestToEntity(DefaultViewsRequest request) {
        DefaultViews view = new DefaultViews();
        modelMapper.map(request, view);
        return view;
    }
}
