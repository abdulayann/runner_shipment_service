package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.dao.interfaces.ICarrierDao;
import com.dpw.runner.shipment.services.dto.request.CarrierDetailRequest;
import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.repository.interfaces.ICarrierRepository;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
@Slf4j
public class CarrierDao implements ICarrierDao {
    @Autowired
    private ICarrierRepository carrierRepository;

    @Override
    public CarrierDetails save(CarrierDetails carrierDetails) {
        return carrierRepository.save(carrierDetails);
    }

    @Override
    public Page<CarrierDetails> findAll(Specification<CarrierDetails> spec, Pageable pageable) {
        return carrierRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<CarrierDetails> findById(Long id) {
        return carrierRepository.findById(id);
    }

    @Override
    public void delete(CarrierDetails carrierDetails) {
        carrierRepository.delete(carrierDetails);
    }

    public CarrierDetails updateEntityFromShipment(CommonRequestModel commonRequestModel, Long shipmentId) throws Exception {
        String responseMsg;
        try {
            // TODO- Handle Transactions here
            CarrierDetailRequest carrierDetailRequest = (CarrierDetailRequest) commonRequestModel.getData();
            if (carrierDetailRequest.getId() != null) {
                long id = carrierDetailRequest.getId();
                Optional<CarrierDetails> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("CarrierDetails is null for Id {}", id);
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
            }
            CarrierDetails carrierDetails = CommonUtils.convertToClass(carrierDetailRequest, CarrierDetails.class);
            carrierDetails = save(carrierDetails);
            return carrierDetails;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }
}
