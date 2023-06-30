package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.TruckDriverDetailsRequest;
import com.dpw.runner.shipment.services.dto.response.TruckDriverDetailsResponse;
import com.dpw.runner.shipment.services.entity.TruckDriverDetails;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.repository.interfaces.ITruckDriverDetailsDao;
import com.dpw.runner.shipment.services.repository.interfaces.ITruckDriverDetailsDao;
import com.dpw.runner.shipment.services.service.interfaces.ITruckDriverDetailsService;
import com.dpw.runner.shipment.services.service.interfaces.ITruckDriverDetailsService;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@Service
@Slf4j
public class TruckDriverDetailsService implements ITruckDriverDetailsService {
    @Autowired
    private ITruckDriverDetailsDao truckDriverDetailsDao;

    @Autowired
    private ModelMapper modelMapper;

    @Override
    @Transactional(propagation = Propagation.MANDATORY)
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) throws Exception {
        TruckDriverDetailsRequest request = (TruckDriverDetailsRequest) commonRequestModel.getData();
        TruckDriverDetails notes = convertRequestToTruckDriverDetailsEntity(request);
        notes = truckDriverDetailsDao.save(notes);
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(notes));
    }

    @Override
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) throws Exception {
        TruckDriverDetailsRequest request = (TruckDriverDetailsRequest) commonRequestModel.getData();
        long id = request.getId();
        Optional<TruckDriverDetails> oldEntity = truckDriverDetailsDao.findById(id);
        if (oldEntity.isEmpty()) {
            log.debug("TruckDriverDetails is null for Id {}", request.getId());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        TruckDriverDetails notes = convertRequestToTruckDriverDetailsEntity(request);
        notes.setId(oldEntity.get().getId());
        notes = truckDriverDetailsDao.save(notes);
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(notes));
    }

    @Override
    public ResponseEntity<?> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();

            Pair<Specification<TruckDriverDetails>, Pageable> tuple = fetchData(request, TruckDriverDetails.class);
            Page<TruckDriverDetails> notesPage = truckDriverDetailsDao.findAll(tuple.getLeft(), tuple.getRight());
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
    public ResponseEntity<?> delete(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            long id = request.getId();
            Optional<TruckDriverDetails> note = truckDriverDetailsDao.findById(id);
            if (note.isEmpty()) {
                log.debug("TruckDriverDetails is null for Id {}", request.getId());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            truckDriverDetailsDao.delete(note.get());
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
            Optional<TruckDriverDetails> notes = truckDriverDetailsDao.findById(id);
            if (notes.isEmpty()) {
                log.debug("TruckDriverDetails is null for Id {}", request.getId());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }

            TruckDriverDetailsResponse response = convertEntityToDto(notes.get());
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private TruckDriverDetailsResponse convertEntityToDto(TruckDriverDetails notes) {
        return modelMapper.map(notes, TruckDriverDetailsResponse.class);
    }

    private TruckDriverDetails convertRequestToTruckDriverDetailsEntity(TruckDriverDetailsRequest request) {
        return modelMapper.map(request, TruckDriverDetails.class);
    }

    private List<IRunnerResponse> convertEntityListToDtoList(final List<TruckDriverDetails> lst) {
        return lst.stream()
                .map(item -> convertEntityToDto(item))
                .collect(Collectors.toList());
    }
}