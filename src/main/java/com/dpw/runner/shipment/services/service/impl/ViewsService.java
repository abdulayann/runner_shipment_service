package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.ViewsRequest;
import com.dpw.runner.shipment.services.dto.response.ViewsResponse;
import com.dpw.runner.shipment.services.entity.DefaultViews;
import com.dpw.runner.shipment.services.entity.TruckDriverDetails;
import com.dpw.runner.shipment.services.entity.Views;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IDefaultViewsDao;
import com.dpw.runner.shipment.services.repository.interfaces.IViewsDao;
import com.dpw.runner.shipment.services.service.interfaces.IViewsService;
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
@Service
@Slf4j
public class ViewsService implements IViewsService {
    @Autowired
    private IViewsDao viewsDao;

    @Autowired
    private IDefaultViewsDao defaultViewsDao;

    @Autowired
    private ModelMapper modelMapper;

    @Transactional
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) {
        String responseMsg;
        ViewsRequest request = null;
        request = (ViewsRequest) commonRequestModel.getData();
        if(request == null) {
            log.debug("Request is empty for Views create");
        }
        Views views = convertRequestToEntity(request);
        try {
            views = viewsDao.save(views);
            log.info("Views Details created successfully for Id {}", views.getId());
            if(request.getIsDefault() == true)
            {
                DefaultViews defaultView = new DefaultViews();
                defaultView.setDefaultViewId(views.getId());
                defaultView.setEntity(views.getEntity());
                defaultView.setUsername(UserContext.getUser().getUserName());
                defaultViewsDao.save(defaultView);
                log.info("Default Views Details created successfully for Id {}", views.getId());
            }
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(views));
    }

    @Transactional
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) {
        String responseMsg;
        ViewsRequest request = (ViewsRequest) commonRequestModel.getData();
        if(request == null) {
            log.error("Request is empty for Views create");
        }

        if(request.getId() == null) {
            log.error("Request Id is null for Views create");
        }
        long id = request.getId();
        Optional<Views> oldEntity = viewsDao.findById(id);
        if(!oldEntity.isPresent()) {
            log.debug("View is null for Id {}", request.getId());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        Views view = convertRequestToEntity(request);
        view.setId(oldEntity.get().getId());
        try {
            view = viewsDao.save(view);
            log.info("Updated the view details for Id {} ", id);
            Optional<DefaultViews> oldDefaultView = defaultViewsDao.findByUsername(UserContext.getUser().getUserName());
            if(oldDefaultView.isPresent())
            {
                if(oldDefaultView.get().getDefaultViewId() != view.getId())
                {
                    oldDefaultView.get().setDefaultViewId(view.getId());
                    defaultViewsDao.save(oldDefaultView.get());
                    log.info("Updated the Default View details for Id {} ", id);
                }
            }
            else
            {
                DefaultViews defaultView = new DefaultViews();
                defaultView.setDefaultViewId(view.getId());
                defaultView.setEntity(view.getEntity());
                defaultView.setUsername(UserContext.getUser().getUserName());
                defaultViewsDao.save(defaultView);
                log.info("Created the Default View details for Id {} ", id);
            }
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(view));
    }

    public ResponseEntity<?> list(CommonRequestModel commonRequestModel){
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if(request == null) {
                log.error("Request is empty for Views Details list");
            }
            List<Views> viewsList = viewsDao.findAll();
            log.info("Views Details list retrieved successfully");
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
    public CompletableFuture<ResponseEntity<?>> listAsync(CommonRequestModel commonRequestModel){
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if(request == null) {
                log.error("Request is empty for Views Details async list");
            }
            List<Views> viewsList = viewsDao.findAll();
            log.info("Views Details async list retrieved successfully");
            return CompletableFuture.completedFuture(
                    ResponseHelper.buildListSuccessResponse(convertEntityListToDtoList(viewsList), request.getPageNo(), viewsList.size()));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return CompletableFuture.completedFuture(ResponseHelper.buildFailedResponse(responseMsg));
        }
    }

    public ResponseEntity<?> delete(CommonRequestModel commonRequestModel){
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if(request == null) {
                log.debug("Request is empty for Views delete");
            }
            if(request.getId() == null) {
                log.debug("Request Id is null for Views delete");
            }
            long id = request.getId();
            Optional<Views> view = viewsDao.findById(id);
            if(!view.isPresent()) {
                log.debug("View is null for Id {}", request.getId());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            if(defaultViewsDao.findByDefaultViewId(view.get().getId()).isPresent())
            {
                defaultViewsDao.delete(defaultViewsDao.findByDefaultViewId(view.get().getId()).get());
            }
            viewsDao.delete(view.get());
            log.info("Deleted views for Id {}", id);
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
            if(request == null) {
                log.error("Request is empty for Views retrieve");
            }
            if(request.getId() == null) {
                log.error("Request Id is null for Views retrieve");
            }
            long id = request.getId();
            Optional<Views> view = viewsDao.findById(id);
            if(!view.isPresent()) {
                log.debug("View is null for Id {}", request.getId());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("Views Details fetched successfully for Id {}", id);
            ViewsResponse response = convertEntityToDto(view.get());
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private ViewsResponse convertEntityToDto(Views view) {
        return modelMapper.map(view, ViewsResponse.class);
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<Views> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(view -> {
            responseList.add(convertEntityToDto(view));
        });
        return responseList;
    }

    public Views convertRequestToEntity(ViewsRequest request) {
        return modelMapper.map(request, Views.class);
    }
}
