package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.OrderNumber;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.Optional;

public interface IOrderNumberDao {
    OrderNumber save(OrderNumber orderNumber);

    Page<OrderNumber> findAll(Specification<OrderNumber> spec, Pageable pageable);

    Optional<OrderNumber> findById(Long id);

    void delete(OrderNumber orderNumber);
}
