package com.dpw.runner.booking.services.dao.interfaces;

import com.dpw.runner.booking.services.entity.Packing;
import com.dpw.runner.booking.services.exception.exceptions.RunnerException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface IPackingDao {
    Packing save(Packing packing);

    Page<Packing> findAll(Specification<Packing> spec, Pageable pageable);

    Optional<Packing> findById(Long id);

    Optional<Packing> findByGuid(UUID id);

    void delete(Packing packing);

    List<Packing> saveEntityFromBooking(List<Packing> packings, Long bookindId);

    List<Packing> updateEntityFromBooking(List<Packing> packings, Long bookingId) throws RunnerException;

    List<Packing> getAllPackings();

    List<Packing> saveAll(List<Packing> packingList);

    List<Packing> findByConsolidationId(Long consolidationId);
}
