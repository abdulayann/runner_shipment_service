package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.ShippingInstruction;
import com.dpw.runner.shipment.services.entity.enums.EntityType;
import com.dpw.runner.shipment.services.projection.CarrierBookingInfoProjection;
import com.dpw.runner.shipment.services.projection.ShippingConsoleIdProjection;
import com.dpw.runner.shipment.services.projection.ShippingConsoleNoProjection;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Optional;

public interface IShippingInstructionDao {

    ShippingInstruction create(ShippingInstruction shippingInstruction);

    Optional<ShippingInstruction> findById(Long id);

    Page<ShippingInstruction> findAll(Specification<ShippingInstruction> spec, Pageable pageable);

    ShippingInstruction update(Long id, ShippingInstruction request);

    void delete(Long id);

    ShippingInstruction save(ShippingInstruction shippingInstruction);

    CarrierBookingInfoProjection findBookingInfoById(Long bookingId);

    boolean existsById(Long id);

    List<ShippingConsoleNoProjection> findByEntityTypeAndEntityNoIn(EntityType entityType, List<String> allConsolNumbers);

    List<ShippingConsoleNoProjection> findByCarrierBookingConsolNumbers(List<String> allConsolNumbers);

    List<ShippingConsoleIdProjection> findByEntityTypeAndEntityIdIn(EntityType entityType, List<Long> allConsolId);

    List<ShippingConsoleIdProjection> findByCarrierBookingConsolId(List<Long> allConsolNumbers);

    List<CarrierBookingInfoProjection> findBookingByConsolId(String consolidationNumber);

}
