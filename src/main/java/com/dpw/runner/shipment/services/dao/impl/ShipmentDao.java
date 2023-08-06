package com.dpw.runner.shipment.services.dao.impl;

import com.azure.messaging.servicebus.ServiceBusMessage;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.ShipmentConstants;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.enums.LifecycleHooks;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IShipmentRepository;
import com.dpw.runner.shipment.services.service_bus.AzureServiceBusTopic;
import com.dpw.runner.shipment.services.service_bus.ISBProperties;
import com.dpw.runner.shipment.services.service_bus.SBUtilsImpl;
import com.dpw.runner.shipment.services.service_bus.model.EventMessage;
import com.dpw.runner.shipment.services.validator.ValidatorUtility;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.*;

@Repository
@Slf4j
public class ShipmentDao implements IShipmentDao {
    @Autowired
    private IShipmentRepository shipmentRepository;

    @Autowired
    private ValidatorUtility validatorUtility;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private SBUtilsImpl sbUtils;

    @Autowired
    private ISBProperties isbProperties;

    @Autowired
    private AzureServiceBusTopic azureServiceBusTopic;

    @Override
    public ShipmentDetails save(ShipmentDetails shipmentDetails) {
        Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(shipmentDetails) , Constants.SHIPMENT, LifecycleHooks.ON_CREATE, false);
        if (! errors.isEmpty())
            throw new ValidationException(errors.toString());
        if(shipmentDetails.getId() != null){
            long id = shipmentDetails.getId();
            Optional<ShipmentDetails> oldEntity = findById(id);
            if (!oldEntity.isPresent()) {
                log.debug("Container is null for Id {}", shipmentDetails.getId());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            if(shipmentDetails.getContainersList() == null) {
                shipmentDetails.setContainersList(oldEntity.get().getContainersList());
            }
            if(shipmentDetails.getConsolidationList() == null) {
                shipmentDetails.setConsolidationList(oldEntity.get().getConsolidationList());
            }
        }
        shipmentDetails = shipmentRepository.save(shipmentDetails);
        EventMessage eventMessage = EventMessage.builder().messageType(Constants.SERVICE).entity(Constants.SHIPMENT).request(shipmentDetails).build();
        sbUtils.sendMessagesToTopic(isbProperties, azureServiceBusTopic.getTopic(), Arrays.asList(new ServiceBusMessage(jsonHelper.convertToJson(eventMessage))));
        return shipmentDetails;
    }

    public List<ShipmentDetails> saveAll(List<ShipmentDetails> shipments)
    {
        List<ShipmentDetails> res = new ArrayList<>();
        for(ShipmentDetails req : shipments){
            req = save(req);
            res.add(req);
        }
        return res;
    }

    @Override
    public ShipmentDetails update(ShipmentDetails shipmentDetails) {
        validateLockStatus(shipmentDetails.getId());
        Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(shipmentDetails) , Constants.SHIPMENT, LifecycleHooks.ON_UPDATE, false);
        if (! errors.isEmpty())
            throw new ValidationException(errors.toString());
        if(shipmentDetails.getId() != null){
            long id = shipmentDetails.getId();
            Optional<ShipmentDetails> oldEntity = findById(id);
            if (!oldEntity.isPresent()) {
                log.debug("Container is null for Id {}", shipmentDetails.getId());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            if(shipmentDetails.getContainersList() == null) {
                shipmentDetails.setContainersList(oldEntity.get().getContainersList());
            }
            if(shipmentDetails.getConsolidationList() == null) {
                shipmentDetails.setConsolidationList(oldEntity.get().getConsolidationList());
            }
        }
        return shipmentRepository.save(shipmentDetails);
    }

    @Override
    public Page<ShipmentDetails> findAll(Specification<ShipmentDetails> spec, Pageable pageable) {
        return shipmentRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<ShipmentDetails> findById(Long id) {
        return shipmentRepository.findById(id);
    }

    @Override
    public void delete(ShipmentDetails shipmentDetails) {
        validateLockStatus(shipmentDetails.getId());
        shipmentRepository.delete(shipmentDetails);
    }

    private void validateLockStatus(Long id) throws ValidationException {
        Optional<ShipmentDetails> existingShipment = findById(id);
        if(existingShipment.get().getIsLocked() != null && existingShipment.get().getIsLocked()) {
            throw new ValidationException(ShipmentConstants.SHIPMENT_LOCKED);
        }
    }
    @Override
    public Optional<ShipmentDetails> findByHouseBill(String Hbl){
        return shipmentRepository.findByHouseBill(Hbl);
    }
}
