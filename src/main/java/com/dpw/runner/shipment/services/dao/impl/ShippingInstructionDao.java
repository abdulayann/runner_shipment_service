package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IShippingInstructionDao;
import com.dpw.runner.shipment.services.entity.ShippingInstruction;
import com.dpw.runner.shipment.services.entity.enums.EntityType;
import com.dpw.runner.shipment.services.projection.CarrierBookingInfoProjection;
import com.dpw.runner.shipment.services.projection.ShippingConsoleIdProjection;
import com.dpw.runner.shipment.services.projection.ShippingConsoleNoProjection;
import com.dpw.runner.shipment.services.repository.interfaces.IShippingInstructionRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Optional;

@Component
public class ShippingInstructionDao implements IShippingInstructionDao {

    @Autowired
    private IShippingInstructionRepository shippingInstructionRepository;

    @Override
    public ShippingInstruction create(ShippingInstruction shippingInstruction) {
        return shippingInstructionRepository.save(shippingInstruction);
    }

    @Override
    public Optional<ShippingInstruction> findById(Long id) {
        return shippingInstructionRepository.findById(id);
    }

    @Override
    public Page<ShippingInstruction> findAll(Specification<ShippingInstruction> spec, Pageable pageable) {
        return shippingInstructionRepository.findAll(spec, pageable);
    }

    @Override
    public ShippingInstruction update(Long id, ShippingInstruction request) {
        return shippingInstructionRepository.save(request);
    }

    @Override
    public void delete(Long id) {
        shippingInstructionRepository.deleteById(id);
    }

    @Override
    public ShippingInstruction save(ShippingInstruction shippingInstruction) {
        return shippingInstructionRepository.save(shippingInstruction);
    }

    @Override
    public CarrierBookingInfoProjection findBookingInfoById(Long bookingId) {
        return shippingInstructionRepository.findCarrierBookingInfoById(bookingId);
    }

    @Override
    public boolean existsById(Long id) {
        return shippingInstructionRepository.existsById(id);
    }

    @Override
    public List<ShippingConsoleNoProjection> findByEntityTypeAndEntityNoIn(EntityType entityType, List<String> allConsolNumbers) {
        return shippingInstructionRepository.findByEntityTypeAndEntityNo(entityType.name(), allConsolNumbers);
    }

    @Override
    public List<ShippingConsoleNoProjection> findByCarrierBookingConsolNumbers(List<String> allConsolNumbers) {
        return shippingInstructionRepository.findByCarrierBookingConsolNumbers(allConsolNumbers);
    }

    @Override
    public List<ShippingConsoleIdProjection> findByEntityTypeAndEntityIdIn(EntityType entityType, List<Long> allConsolId) {
        return shippingInstructionRepository.findByEntityTypeAndEntityId(entityType.name(), allConsolId);
    }

    @Override
    public List<ShippingConsoleIdProjection> findByCarrierBookingConsolId(List<Long> allConsolNumbers) {
        return shippingInstructionRepository.findByCarrierBookingConsolId(allConsolNumbers);
    }

    @Override
    public List<CarrierBookingInfoProjection> findBookingByConsolId(String consolidationNumber) {
        return shippingInstructionRepository.findByConsolidationNo(consolidationNumber);
    }


}
