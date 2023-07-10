package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.PackingConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.BookingCarriageRequest;
import com.dpw.runner.shipment.services.dto.request.JobRequest;
import com.dpw.runner.shipment.services.dto.request.PackingRequest;
import com.dpw.runner.shipment.services.dto.response.PackingResponse;
import com.dpw.runner.shipment.services.entity.BookingCarriage;
import com.dpw.runner.shipment.services.entity.Jobs;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IPackingDao;
import com.dpw.runner.shipment.services.service.interfaces.IPackingService;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.bouncycastle.util.Pack;
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

@Slf4j
@Service
public class PackingService implements IPackingService {
    @Autowired
    IPackingDao packingDao;
    @Autowired
    ModelMapper modelMapper;

    @Transactional
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) {
        String responseMsg;
        PackingRequest request = null;
        request = (PackingRequest) commonRequestModel.getData();
        if(request == null) {
            log.debug("Request is empty for Packing create");
        }
        Packing packing = convertRequestToEntity(request);
        try {
            packing = packingDao.save(packing);
            log.info("Packing Details created successfully for Id {}", packing.getId());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(packing));
    }

    @Transactional
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) {
        String responseMsg;
        PackingRequest request = (PackingRequest) commonRequestModel.getData();
        if(request == null) {
            log.debug("Request is empty for Packing update");
        }

        if(request.getId() == null) {
            log.debug("Request Id is null for Packing update");
        }
        long id = request.getId();
        Optional<Packing> oldEntity = packingDao.findById(id);
        if (!oldEntity.isPresent()) {
            log.debug("Packing is null for Id {}", request.getId());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        Packing packing = convertRequestToEntity(request);
        packing.setId(oldEntity.get().getId());
        try {
            packing = packingDao.save(packing);
            log.info("Updated the packing details for Id {} ", id);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(packing));
    }

    public ResponseEntity<?> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if(request == null) {
                log.error("Request is empty for Packing list");
            }
            // construct specifications for filter request
            Pair<Specification<Packing>, Pageable> tuple = fetchData(request, BookingCarriage.class);
            Page<Packing> packingPage = packingDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Packing list retrieved successfully");
            return ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(packingPage.getContent()),
                    packingPage.getTotalPages(),
                    packingPage.getTotalElements());
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
                log.error("Request is empty for Packing async list");
            }
            // construct specifications for filter request
            Pair<Specification<Packing>, Pageable> tuple = fetchData(request, Packing.class);
            Page<Packing> packingPage = packingDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Packing async list retrieved successfully");
            return CompletableFuture.completedFuture(
                    ResponseHelper
                            .buildListSuccessResponse(
                                    convertEntityListToDtoList(packingPage.getContent()),
                                    packingPage.getTotalPages(),
                                    packingPage.getTotalElements()));
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
        if(commonRequestModel == null) {
            log.debug("Request is empty for Packing delete");
        }
        if(commonRequestModel.getId() == null) {
            log.debug("Request Id is null for Packing delete");
        }
        Long id = commonRequestModel.getId();

        Optional<Packing> targetPacking = packingDao.findById(id);
        if (targetPacking.isEmpty()) {
            log.debug("No entity present for id {} ", id);
            return ResponseHelper.buildFailedResponse(PackingConstants.NO_DATA);
        }
        try {
            packingDao.delete(targetPacking.get());
            log.info("Deleted packing for Id {}", id);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse();
    }

    @Override
    public ResponseEntity<?> retrieveById(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if(request == null) {
                log.error("Request is empty for Packing retrieve");
            }
            if(request.getId() == null) {
                log.error("Request Id is null for Packing retrieve");
            }
            long id = request.getId();
            Optional<Packing> packing = packingDao.findById(id);
            if (packing.isEmpty()) {
                log.debug("Packing is null for Id {}", request.getId());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("Packing details fetched successfully for Id {}", id);
            PackingResponse response = (PackingResponse) convertEntityToDto(packing.get());
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private IRunnerResponse convertEntityToDto(Packing packing) {
        return modelMapper.map(packing, PackingResponse.class);
    }

    private Packing convertRequestToEntity(PackingRequest request) {
        return modelMapper.map(request, Packing.class);
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<Packing> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(packing -> {
            responseList.add(convertEntityToDto(packing));
        });
        return responseList;
    }

}
