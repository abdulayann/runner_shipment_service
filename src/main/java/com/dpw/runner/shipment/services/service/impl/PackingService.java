package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.PackingConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.PackingRequest;
import com.dpw.runner.shipment.services.dto.response.PackingResponse;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IPackingDao;
import com.dpw.runner.shipment.services.service.interfaces.IPackingService;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.TransactionDefinition;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.support.DefaultTransactionDefinition;

import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Slf4j
@Service
public class PackingService implements IPackingService {
    @Autowired
    IPackingDao packingDao;
    @Autowired
    ModelMapper modelMapper;

    @Autowired
    private PlatformTransactionManager transactionManager;

    @Override
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) throws Exception {
        TransactionDefinition txDef = new DefaultTransactionDefinition();
        var txStatus = transactionManager.getTransaction(txDef);
        try {
            PackingRequest request = (PackingRequest) commonRequestModel.getData();
            Packing packing = convertRequestToEntity(request);
            packing = packingDao.save(packing);
            transactionManager.commit(txStatus);
            return ResponseHelper.buildSuccessResponse(convertEntityToDto(packing));
        } catch (Exception ex) {
            log.error("Create Packing Call Failed");
            transactionManager.rollback(txStatus);
            return ResponseHelper.buildFailedResponse("Create Packing Call Failed");
        }
    }

    @Transactional
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) throws Exception {
        PackingRequest request = (PackingRequest) commonRequestModel.getData();
        Packing updatedEntity = convertRequestToEntity(request);
        packingDao.save(updatedEntity);
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(updatedEntity));
    }

    @Override
    public ResponseEntity<?> list(CommonRequestModel commonRequestModel) {
        String responseMessage;
        try {
            Long id = commonRequestModel.getId();
            List<Packing> packings = packingDao.findByShipmentId(id);
            List<IRunnerResponse> response = packings.stream()
                    .map(this::convertEntityToDto)
                    .collect(Collectors.toList());
            return ResponseHelper.buildListSuccessResponse(response);
        } catch (Exception e) {
            responseMessage = e.getMessage();
            log.debug(responseMessage);
        }
        return ResponseHelper.buildFailedResponse(responseMessage);
    }

    @Override
    public ResponseEntity<?> delete(CommonRequestModel commonRequestModel) {
        Long id = commonRequestModel.getId();
        Optional<Packing> targetPacking = packingDao.findById(id);
        if (targetPacking.isEmpty()) {
            log.debug("No entity present for id {} ", id);
            return ResponseHelper.buildFailedResponse(PackingConstants.NO_DATA);
        }
        packingDao.delete(targetPacking.get());
        return ResponseHelper.buildSuccessResponse();
    }

    @Override
    public ResponseEntity<?> retrieveById(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            long id = request.getId();
            Optional<Packing> packing = packingDao.findById(id);
            if (packing.isEmpty()) {
                log.debug("Packing is null for Id {}", request.getId());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }

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

}
