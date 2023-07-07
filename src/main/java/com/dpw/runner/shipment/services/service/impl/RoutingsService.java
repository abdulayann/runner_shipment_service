package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.RoutingsRequest;
import com.dpw.runner.shipment.services.dto.response.RoutingsResponse;
import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IRoutingsDao;
import com.dpw.runner.shipment.services.service.interfaces.IRoutingsService;
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
import org.springframework.transaction.annotation.Propagation;
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
public class RoutingsService implements IRoutingsService {
    @Autowired
    private IRoutingsDao routingsDao;

    @Autowired
    private ModelMapper modelMapper;

    @Override
    @Transactional
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) {
        RoutingsRequest request = (RoutingsRequest) commonRequestModel.getData();
        Routings notes = convertRequestToRoutingsEntity(request);
        notes = routingsDao.save(notes);
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(notes));
    }

    @Override
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) {
        RoutingsRequest request = (RoutingsRequest) commonRequestModel.getData();
        long id = request.getId();
        Optional<Routings> oldEntity = routingsDao.findById(id);
        if (oldEntity.isEmpty()) {
            log.debug("Routings is null for Id {}", request.getId());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        Routings notes = convertRequestToRoutingsEntity(request);
        notes.setId(oldEntity.get().getId());
        notes = routingsDao.save(notes);
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(notes));
    }

    @Override
    public ResponseEntity<?> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();

            Pair<Specification<Routings>, Pageable> tuple = fetchData(request, Routings.class);
            Page<Routings> notesPage = routingsDao.findAll(tuple.getLeft(), tuple.getRight());
            return ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(notesPage.getContent()),
                    notesPage.getTotalPages(),
                    notesPage.getTotalElements());
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

            Pair<Specification<Routings>, Pageable> tuple = fetchData(request, Routings.class);
            Page<Routings> notesPage = routingsDao.findAll(tuple.getLeft(), tuple.getRight());
            return CompletableFuture.completedFuture(ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(notesPage.getContent()),
                    notesPage.getTotalPages(),
                    notesPage.getTotalElements()));
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
            long id = request.getId();
            Optional<Routings> note = routingsDao.findById(id);
            if (note.isEmpty()) {
                log.debug("Routings is null for Id {}", request.getId());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            routingsDao.delete(note.get());
            return ResponseHelper.buildSuccessResponse();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<?> retrieveById(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            long id = request.getId();
            Optional<Routings> notes = routingsDao.findById(id);
            if (notes.isEmpty()) {
                log.debug("Routings is null for Id {}", request.getId());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }

            RoutingsResponse response = convertEntityToDto(notes.get());
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
        List<Routings> responseRoutings = new ArrayList<>();
        try {
            // TODO- Handle Transactions here
            ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentId", shipmentId, "=");
            Pair<Specification<Routings>, Pageable> pair = fetchData(listCommonRequest, Routings.class);
            Page<Routings> routings = routingsDao.findAll(pair.getLeft(), pair.getRight());
            HashSet<Long> existingIds = new HashSet<>(routings
                    .stream()
                    .map(Routings::getId)
                    .collect(Collectors.toList()));
            List<RoutingsRequest> routingsRequestList = new ArrayList<>();
            List<RoutingsRequest> requestList = (List<RoutingsRequest>) commonRequestModel.getDataList();
            if (requestList != null && requestList.size() != 0) {
                for (RoutingsRequest request : requestList) {
                    Long id = request.getId();
                    if (id != null) {
                        existingIds.remove(id);
                    }
                    routingsRequestList.add(request);
                }
                responseRoutings = saveRoutings(routingsRequestList);
            }
            deleteRoutings(existingIds);
            return ResponseHelper.buildListSuccessResponse(convertEntityListToDtoList(responseRoutings));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private List<Routings> saveRoutings(List<RoutingsRequest> routings) {
        List<Routings> res = new ArrayList<>();
        for(RoutingsRequest req : routings){
            Routings saveEntity = convertRequestToRoutingsEntity(req);
            if(req.getId() != null){
                long id = req.getId();
                Optional<Routings> oldEntity = routingsDao.findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("Booking Carriage is null for Id {}", req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
                saveEntity = oldEntity.get();
            }
            saveEntity = routingsDao.save(saveEntity);
            res.add(saveEntity);
        }
        return res;
    }

    private ResponseEntity<?> deleteRoutings(HashSet<Long> existingIds) {
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

    private RoutingsResponse convertEntityToDto(Routings notes) {
        return modelMapper.map(notes, RoutingsResponse.class);
    }

    private Routings convertRequestToRoutingsEntity(RoutingsRequest request) {
        return modelMapper.map(request, Routings.class);
    }

    private List<IRunnerResponse> convertEntityListToDtoList(final List<Routings> lst) {
        return lst.stream()
                .map(item -> convertEntityToDto(item))
                .collect(Collectors.toList());
    }
}