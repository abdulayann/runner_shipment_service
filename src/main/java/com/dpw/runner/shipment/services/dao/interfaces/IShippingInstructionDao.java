package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.ShippingInstruction;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.Optional;

public interface IShippingInstructionDao {
    ShippingInstruction save(ShippingInstruction shippingInstruction);

    Page<ShippingInstruction> findAll(Specification<ShippingInstruction> spec, Pageable pageable);

    Optional<ShippingInstruction> findById(Long id);

    void delete(ShippingInstruction shippingInstruction);
}
