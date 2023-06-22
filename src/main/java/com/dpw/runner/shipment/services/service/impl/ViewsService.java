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
import com.dpw.runner.shipment.services.entity.Views;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IDefaultViewsDao;
import com.dpw.runner.shipment.services.repository.interfaces.IViewsDao;
import com.dpw.runner.shipment.services.service.interfaces.IViewsService;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

@SuppressWarnings("ALL")
@Service
@Slf4j
public class ViewsService implements IViewsService {
    @Autowired
    private IViewsDao viewsDao;

    @Autowired
    private IDefaultViewsDao defaultViewsDao;

    @Transactional
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) throws Exception {
        ViewsRequest request = null;
        request = (ViewsRequest) commonRequestModel.getData();
        Views views = convertRequestToEntity(request);
        views = viewsDao.save(views);
        if(request.getIsDefault() == true)
        {
            DefaultViews defaultView = new DefaultViews();
            defaultView.setDefaultViewId(views.getId());
            defaultView.setEntity(views.getEntity());
            defaultView.setUsername(UserContext.getUser().getUserName());
            defaultViewsDao.save(defaultView);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(views));
    }

    @Transactional
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) throws Exception {
        ViewsRequest request = (ViewsRequest) commonRequestModel.getData();
        long id =request.getId();
        Optional<Views> oldEntity = viewsDao.findById(id);
        if(!oldEntity.isPresent()) {
            log.debug("View is null for Id {}", request.getId());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        Views view = convertRequestToEntity(request);
        view.setId(oldEntity.get().getId());
        view = viewsDao.save(view);
        Optional<DefaultViews> oldDefaultView = defaultViewsDao.findByUsername(UserContext.getUser().getUserName());
        if(oldDefaultView.isPresent())
        {
            if(oldDefaultView.get().getDefaultViewId() != view.getId())
            {
                oldDefaultView.get().setDefaultViewId(view.getId());
                defaultViewsDao.save(oldDefaultView.get());
            }
        }
        else
        {
            DefaultViews defaultView = new DefaultViews();
            defaultView.setDefaultViewId(view.getId());
            defaultView.setEntity(view.getEntity());
            defaultView.setUsername(UserContext.getUser().getUserName());
            defaultViewsDao.save(defaultView);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(view));
    }

    public ResponseEntity<?> list(CommonRequestModel commonRequestModel){
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            List<Views> viewsList = viewsDao.findAll();

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
            Optional<Views> view = viewsDao.findById(id);
            if(!view.isPresent()) {
                log.debug("View is null for Id {}", request.getId());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }

            ViewsResponse response = convertEntityToDto(view.get());
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private static ViewsResponse convertEntityToDto(Views view) {
        ViewsResponse response = new ViewsResponse();
        new ModelMapper().map(view, response);
        return response;
    }

    private static List<IRunnerResponse> convertEntityListToDtoList(List<Views> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(view -> {
            responseList.add(convertEntityToDto(view));
        });
        return responseList;
    }

    public static Views convertRequestToEntity(ViewsRequest request) {
        Views view = new Views();
        new ModelMapper().map(request, view);
        return view;
    }
}
