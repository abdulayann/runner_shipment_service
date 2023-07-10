package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.PartiesRequest;
import com.dpw.runner.shipment.services.dto.response.PartiesResponse;
import com.dpw.runner.shipment.services.entity.BookingCarriage;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IPartiesDao;
import com.dpw.runner.shipment.services.service.interfaces.IPartiesDetailsService;
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
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@Service
@Slf4j
public class PartiesService implements IPartiesDetailsService {
    @Autowired
    private IPartiesDao partiesDao;

    @Autowired
    private ModelMapper modelMapper;

    @Override
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) {
        String responseMsg;
        PartiesRequest request = (PartiesRequest) commonRequestModel.getData();
        if(request == null) {
            log.debug("Request is empty for Parties create");
        }
        Parties notes = convertRequestToPartiesDetailsEntity(request);
        try {
            notes = partiesDao.save(notes);
            log.info("Parties Details created successfully for Id {}", notes.getId());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(notes));
    }

    @Override
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) {
        String responseMsg;
        PartiesRequest request = (PartiesRequest) commonRequestModel.getData();
        if(request == null) {
            log.debug("Request is empty for Parties update");
        }

        if(request.getId() == null) {
            log.debug("Request Id is null for Parties update");
        }
        long id = request.getId();
        Optional<Parties> oldEntity = partiesDao.findById(id);
        if (oldEntity.isEmpty()) {
            log.debug("Parties Details is null for Id {}", request.getId());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        Parties notes = convertRequestToPartiesDetailsEntity(request);
        notes.setId(oldEntity.get().getId());
        try {
            notes = partiesDao.save(notes);
            log.info("Updated the Parties details for Id {} ", id);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(notes));
    }

    @Override
    public ResponseEntity<?> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if(request == null) {
                log.error("Request is empty for Parties list");
            }
            Pair<Specification<Parties>, Pageable> tuple = fetchData(request, Parties.class);
            Page<Parties> notesPage = partiesDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Parties list retrieved successfully");
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
    public CompletableFuture<ResponseEntity<?>> listAsync(CommonRequestModel commonRequestModel){
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if(request == null) {
                log.error("Request is empty for Parties async list");
            }
            Pair<Specification<Parties>, Pageable> tuple = fetchData(request, Parties.class);
            Page<Parties> partiesPage = partiesDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Parties async list retrieved successfully");
            return CompletableFuture.completedFuture( ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(partiesPage.getContent()),
                    partiesPage.getTotalPages(),
                    partiesPage.getTotalElements()));
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
                log.debug("Request is empty for Parties delete");
            }
            if(request.getId() == null) {
                log.debug("Request Id is null for Parties delete");
            }
            long id = request.getId();

            Optional<Parties> note = partiesDao.findById(id);
            if (note.isEmpty()) {
                log.debug("PartiesDetails is null for Id {}", request.getId());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            partiesDao.delete(note.get());
            log.info("Deleted party detail for Id {}", id);
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
            if(request == null) {
                log.error("Request is empty for Parties details retrieve");
            }
            if(request.getId() == null) {
                log.error("Request Id is null for Parties details retrieve");
            }
            long id = request.getId();
            Optional<Parties> notes = partiesDao.findById(id);
            if (notes.isEmpty()) {
                log.debug("PartiesDetails is null for Id {}", request.getId());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("Parties details fetched successfully for Id {}", id);
            PartiesResponse response = convertEntityToDto(notes.get());
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private PartiesResponse convertEntityToDto(Parties notes) {
        return modelMapper.map(notes, PartiesResponse.class);
    }

    private Parties convertRequestToPartiesDetailsEntity(PartiesRequest request) {
        return modelMapper.map(request, Parties.class);
    }

    private List<IRunnerResponse> convertEntityListToDtoList(final List<Parties> lst) {
        return lst.stream()
                .map(item -> convertEntityToDto(item))
                .collect(Collectors.toList());
    }

//    private List<IRunnerResponse> convertEntityListToDtoList(List<Parties> lst) {
//        List<IRunnerResponse> responseList = new ArrayList<>();
//        lst.forEach(party -> {
//            responseList.add(convertEntityToDto(party));
//        });
//        return responseList;
//    }
}