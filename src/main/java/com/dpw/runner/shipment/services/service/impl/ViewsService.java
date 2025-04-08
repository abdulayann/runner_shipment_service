package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.ViewsConstants;
import com.dpw.runner.shipment.services.commons.requests.*;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IViewsDao;
import com.dpw.runner.shipment.services.dto.request.ViewsRequest;
import com.dpw.runner.shipment.services.dto.response.ViewsResponse;
import com.dpw.runner.shipment.services.entity.Views;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IViewsService;
import com.dpw.runner.shipment.services.utils.PartialFetchUtils;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
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

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@SuppressWarnings("ALL")
@Service
@Slf4j
public class ViewsService implements IViewsService {
    @Autowired
    private IViewsDao viewsDao;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private PartialFetchUtils partialFetchUtils;

    private Map<String, RunnerEntityMapping> tableNames = Map.ofEntries(
            Map.entry("entity", RunnerEntityMapping.builder().tableName(Constants.VIEWS).dataType(String.class).fieldName(Constants.ENTITY).isContainsText(true).build()),
            Map.entry(ViewsConstants.CREATED_BY, RunnerEntityMapping.builder().tableName(Constants.VIEWS).dataType(String.class).fieldName(Constants.CREATED_BY).isContainsText(true).build()),
            Map.entry("name", RunnerEntityMapping.builder().tableName(Constants.VIEWS).dataType(String.class).fieldName(Constants.NAME_FILTER).isContainsText(true).build()),
            Map.entry("isDefault", RunnerEntityMapping.builder().tableName(Constants.VIEWS).dataType(Boolean.class).fieldName(Constants.NAME_FILTER).build())
    );

    @Transactional
    public ResponseEntity<IRunnerResponse> create(CommonRequestModel commonRequestModel) {
        String responseMsg;
        ViewsRequest request = null;
        request = (ViewsRequest) commonRequestModel.getData();
        List<String> viewsNamesList = viewsDao.findAllByUsername(UserContext.getUser().getUsername());
        if(viewsNamesList != null && viewsNamesList.contains(request.getName()))
        {
            throw new ValidationException("A view with this name already exists, please change the view name!");
        }
        if(Boolean.TRUE.equals(request.getIsDefault())) {
            Optional<Views> view = viewsDao.findByCreatedByAndEntityAndIsDefault(UserContext.getUser().getUsername(), request.getEntity());
            if (view.isPresent()) {
                Views viewUpdate = view.get();
                viewUpdate.setIsDefault(false);
                viewsDao.save(viewUpdate);
            }
        }
        Views views = convertRequestToEntity(request);
        try {
            views = viewsDao.save(views);
            log.info("Views Details created successfully for Id {} with Request Id {}", views.getId(), LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(views));
    }

    @Transactional
    public ResponseEntity<IRunnerResponse> update(CommonRequestModel commonRequestModel) {
        String responseMsg;
        ViewsRequest request = (ViewsRequest) commonRequestModel.getData();

        if(request.getId() == null) {
            log.error("Request Id is null for Views create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        Long id = request.getId();
        Optional<Views> oldEntity = viewsDao.findById(id);
        if(!oldEntity.isPresent()) {
            log.debug(ViewsConstants.VIEWS_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        if(!Objects.equals(oldEntity.get().getCreatedBy(), UserContext.getUser().getUsername()))
        {
            throw new ValidationException("This view does not belongs to this user");
        }
        if(!Objects.equals(oldEntity.get().getEntity(), request.getEntity()))
        {
            throw new ValidationException("Entity of the view cannot be changed.");
        }
        if(!Objects.equals(oldEntity.get().getName(), request.getName()))
        {
            List<String> viewsNamesList = viewsDao.findAllByUsername(UserContext.getUser().getUsername());
            if(viewsNamesList != null && viewsNamesList.contains(request.getName()))
            {
                throw new ValidationException("A view with this name already exists, please change the view name!");
            }
        }
        if(!Boolean.TRUE.equals(oldEntity.get().getIsDefault()) && Boolean.TRUE.equals(request.getIsDefault())) {
            Optional<Views> view = viewsDao.findByCreatedByAndEntityAndIsDefault(UserContext.getUser().getUsername(), request.getEntity());
            if (view.isPresent()) {
                Views viewUpdate = view.get();
                viewUpdate.setIsDefault(false);
                viewsDao.save(viewUpdate);
            }
        }
        Views view = convertRequestToEntity(request);
        try {
            view = viewsDao.save(view);
            log.info("Updated the view details for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(view));
    }

    public ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel){
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            var criteria = request.getFilterCriteria();
            if(criteria != null && !criteria.isEmpty())
            {
                var filterCriteria = criteria.get(0);
                if(filterCriteria != null && filterCriteria.getInnerFilter() != null && !filterCriteria.getInnerFilter().isEmpty())
                {
                    filterCriteria.getInnerFilter().add(FilterCriteria.builder().
                            logicOperator("AND").
                            criteria(Criteria.builder()
                                    .fieldName(ViewsConstants.CREATED_BY)
                                    .operator("=")
                                    .value(UserContext.getUser().Username)
                                    .build()).
                            build());
                }
                else
                {
                    filterCriteria = new FilterCriteria();
                    filterCriteria.setInnerFilter(List.of(FilterCriteria.builder().
                            criteria(Criteria.builder()
                                    .fieldName(ViewsConstants.CREATED_BY)
                                    .operator("=")
                                    .value(UserContext.getUser().Username)
                                    .build()).
                            build()));
                }
                criteria.set(0, filterCriteria);
            }
            else
            {
                criteria = new ArrayList<>();
                criteria.add(FilterCriteria.builder().
                        criteria(Criteria.builder()
                                .fieldName(ViewsConstants.CREATED_BY)
                                .operator("=")
                                .value(UserContext.getUser().Username)
                                .build()).
                        build());
            }
            request.setFilterCriteria(criteria);
            Pair<Specification<Views>, Pageable> tuple = fetchData(request, Views.class, tableNames);
            Page<Views> viewsPage = viewsDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Views Details list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildListSuccessResponse(convertEntityListToDtoList(viewsPage.getContent()), viewsPage.getTotalPages(), viewsPage.getTotalElements());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    @Async
    public CompletableFuture<ResponseEntity<IRunnerResponse>> listAsync(CommonRequestModel commonRequestModel){
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            List<Views> viewsList = viewsDao.findAll();
            log.info("Views Details async list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return CompletableFuture.completedFuture(
                    ResponseHelper.buildListSuccessResponse(convertEntityListToDtoList(viewsList), request.getPageNo(), viewsList.size()));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return CompletableFuture.completedFuture(ResponseHelper.buildFailedResponse(responseMsg));
        }
    }

    public ResponseEntity<IRunnerResponse> delete(CommonRequestModel commonRequestModel){
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if(request.getId() == null) {
                log.debug("Request Id is null for Views delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            Long id = request.getId();
            Optional<Views> view = viewsDao.findById(id);
            if(!view.isPresent()) {
                log.debug(ViewsConstants.VIEWS_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            viewsDao.delete(view.get());
            log.info("Deleted views for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildSuccessResponse();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public ResponseEntity<IRunnerResponse> retrieveById(CommonRequestModel commonRequestModel){
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if(request.getId() == null) {
                log.error("Request Id is null for Views retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new ValidationException("Views Retrieve failed because Id is null.");
            }
            long id = request.getId();
            Optional<Views> view = viewsDao.findById(id);
            if(!view.isPresent()) {
                log.debug(ViewsConstants.VIEWS_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("Views Details fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            ViewsResponse response = convertEntityToDto(view.get());
            if(request.getIncludeColumns()==null || request.getIncludeColumns().isEmpty())
                return ResponseHelper.buildSuccessResponse(response);
            else return ResponseHelper.buildSuccessResponse(partialFetchUtils.fetchPartialListData(response, request.getIncludeColumns()));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private ViewsResponse convertEntityToDto(Views view) {
        return jsonHelper.convertValue(view, ViewsResponse.class);
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<Views> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(view -> responseList.add(convertEntityToDto(view)));
        return responseList;
    }

    public Views convertRequestToEntity(ViewsRequest request) {
        return jsonHelper.convertValue(request, Views.class);
    }
}
