package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.dao.interfaces.IShippingInstructionDao;
import com.dpw.runner.shipment.services.entity.ShippingInstruction;
import com.dpw.runner.shipment.services.entity.enums.LifecycleHooks;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IShippingInstructionRepository;
import com.dpw.runner.shipment.services.validator.ValidatorUtility;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

@Repository
@Slf4j
public class ShippingInstructionDao implements IShippingInstructionDao {
    @Autowired
    private IShippingInstructionRepository shippingInstructionRepository;

    @Autowired
    private ValidatorUtility validatorUtility;

    @Autowired
    private JsonHelper jsonHelper;

    @Override
    public ShippingInstruction save(ShippingInstruction shippingInstruction) {

        Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(shippingInstruction) , Constants.SHIPPING_INSTRUCTION, LifecycleHooks.ON_CREATE, false);
        if (! errors.isEmpty())
            throw new ValidationException(String.join(",", errors));
        if (shippingInstruction.getId() != null) {
            Optional<ShippingInstruction> oldEntity = findById(shippingInstruction.getId());
            if (oldEntity.isEmpty()) {
                log.debug("Shipping Instruction is null for Id {}", shippingInstruction.getId());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
        }
        return shippingInstructionRepository.save(shippingInstruction);
    }

    @Override
    public Page<ShippingInstruction> findAll(Specification<ShippingInstruction> spec, Pageable pageable) {
        return shippingInstructionRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<ShippingInstruction> findById(Long id) {
        return shippingInstructionRepository.findById(id);
    }

    @Override
    public Optional<ShippingInstruction> findByGuid(UUID id) {
        return shippingInstructionRepository.findByGuid(id);
    }

    @Override
    public void delete(ShippingInstruction shippingInstruction) {
        shippingInstructionRepository.delete(shippingInstruction);
    }

    public List<ShippingInstruction> saveAll(List<ShippingInstruction> shippingInstructionList) {
        for(var shippingInstruction : shippingInstructionList){
            Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(shippingInstruction), Constants.SHIPPING_INSTRUCTION, LifecycleHooks.ON_CREATE, false);
            if (!errors.isEmpty())
                throw new ValidationException(String.join(",", errors));
        }
        return shippingInstructionRepository.saveAll(shippingInstructionList);
    }
}
