package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.requests.RunnerEntityMapping;
import com.dpw.runner.shipment.services.dto.mapper.ELDetailsMapper;
import com.dpw.runner.shipment.services.dto.request.ELDetailsRequest;
import com.dpw.runner.shipment.services.dto.request.ElNumbersRequest;
import com.dpw.runner.shipment.services.dto.response.ELDetailsResponse;
import com.dpw.runner.shipment.services.entity.ELDetails;
import com.dpw.runner.shipment.services.entity.enums.MergeClass;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IELDetailsDao;
import com.dpw.runner.shipment.services.service.interfaces.IELDetailsService;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Map;
import java.util.Optional;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@Service
@Slf4j
public class ELDetailsService implements IELDetailsService {
    @Autowired
    private IELDetailsDao elDetailsDao;

    @Transactional
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) throws Exception {
        ELDetailsRequest request = (ELDetailsRequest) commonRequestModel.getData();
        ELDetails elDetails = ELDetailsMapper.convertRequestToELDetails(request);
        elDetails = elDetailsDao.save(elDetails);
        return ResponseHelper.buildSuccessResponse(ELDetailsMapper.convertEntityToDto(elDetails));
    }

    @Transactional
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) throws Exception {
        ELDetailsRequest request = (ELDetailsRequest) commonRequestModel.getData();
        long id = request.getId();
        Optional<ELDetails> oldEntity = elDetailsDao.findById(id);
        if (oldEntity.isEmpty()) {
            log.debug("EL Details is null for Id {}", request.getId());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        ELDetails elDetails = ELDetailsMapper.convertRequestToELDetails(request);
        elDetails.setId(oldEntity.get().getId());
        elDetails = elDetailsDao.save(elDetails);
        return ResponseHelper.buildSuccessResponse(ELDetailsMapper.convertEntityToDto(elDetails));
    }

    @Override
    public ResponseEntity<?> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();

            Pair<Specification<ELDetails>, Pageable> tuple = fetchData(request, ELDetails.class, tableNames);
            Page<ELDetails> elDetailsPage = elDetailsDao.findAll(tuple.getLeft(), tuple.getRight());
            return ResponseHelper.buildListSuccessResponse(
                    ELDetailsMapper.convertEntityListToDtoList(elDetailsPage.getContent()),
                    elDetailsPage.getTotalPages(),
                    elDetailsPage.getTotalElements());
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
            Optional<ELDetails> elDetails = elDetailsDao.findById(id);
            if (!elDetails.isPresent()) {
                log.debug("El Details is null for Id {}", request.getId());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            elDetailsDao.delete(elDetails.get());
            return ResponseHelper.buildSuccessResponse();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<?> validateElNumber(CommonRequestModel requestModel) {
        String responseMsg;
        try {
            ElNumbersRequest request = (ElNumbersRequest) (requestModel.getData());
            String elNumber = request.getElNumber();
            Optional<ELDetails> elDetails = elDetailsDao.findByElNumber(elNumber);
            if (elDetails.isEmpty()) {
                log.debug("El Detail is null for El Number {}", request.getElNumber());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            ELDetailsResponse response = ELDetailsMapper.convertEntityToDto(elDetails.get());
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
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
            Optional<ELDetails> elDetail = elDetailsDao.findById(id);
            if (!elDetail.isPresent()) {
                log.debug("El Detail is null for Id {}", request.getId());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }

            ELDetailsResponse response = ELDetailsMapper.convertEntityToDto(elDetail.get());
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private static Map<String, RunnerEntityMapping> tableNames = Map.ofEntries(
            Map.entry("shipmentId", RunnerEntityMapping.builder().tableName("ELDetails").dataType(Long.class).build()),
            Map.entry("elNumber", RunnerEntityMapping.builder().tableName("ELDetails").dataType(String.class).build()),
            Map.entry("packages", RunnerEntityMapping.builder().tableName("ELDetails").dataType(Long.class).build()),
            Map.entry("unit", RunnerEntityMapping.builder().tableName("ELDetails").dataType(String.class).build()),
            Map.entry("weight", RunnerEntityMapping.builder().tableName("ELDetails").dataType(Long.class).build()),
            Map.entry("weightUnit", RunnerEntityMapping.builder().tableName("ELDetails").dataType(String.class).build()),
            Map.entry("mergeClass", RunnerEntityMapping.builder().tableName("ELDetails").dataType(MergeClass.class).build()),
            Map.entry("mergePackage", RunnerEntityMapping.builder().tableName("ELDetails").dataType(Long.class).build()),
            Map.entry("mergePackageUnit", RunnerEntityMapping.builder().tableName("ELDetails").dataType(String.class).build()),
            Map.entry("partition", RunnerEntityMapping.builder().tableName("ELDetails").dataType(Boolean.class).build()),
            Map.entry("partitionSeqNumber", RunnerEntityMapping.builder().tableName("ELDetails").dataType(Long.class).build())
    );

}
