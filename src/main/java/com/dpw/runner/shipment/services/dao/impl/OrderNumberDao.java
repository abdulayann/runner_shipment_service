package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IOrderNumberDao;
import com.dpw.runner.shipment.services.entity.OrderNumber;
import com.dpw.runner.shipment.services.repository.interfaces.IOrderNumberRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
@Slf4j
public class OrderNumberDao implements IOrderNumberDao {
    @Autowired
    private IOrderNumberRepository OrderNumberRepository;

    @Override
    public OrderNumber save(OrderNumber OrderNumber) {
        return OrderNumberRepository.save(OrderNumber);
    }

    @Override
    public Page<OrderNumber> findAll(Specification<OrderNumber> spec, Pageable pageable) {
        return OrderNumberRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<OrderNumber> findById(Long id) {
        return OrderNumberRepository.findById(id);
    }

    @Override
    public void delete(OrderNumber OrderNumber) {
        OrderNumberRepository.delete(OrderNumber);
    }
}
