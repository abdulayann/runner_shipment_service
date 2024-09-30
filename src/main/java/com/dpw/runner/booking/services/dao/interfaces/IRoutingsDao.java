package com.dpw.runner.booking.services.dao.interfaces;

import com.dpw.runner.booking.services.entity.Routings;
import com.dpw.runner.booking.services.exception.exceptions.RunnerException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface IRoutingsDao {
    Routings save(Routings routings);
    List<Routings> saveAll(List<Routings> routingsList);

    Page<Routings> findAll(Specification<Routings> spec, Pageable pageable);

    Optional<Routings> findById(Long id);

    Optional<Routings> findByGuid(UUID id);

    void delete(Routings routings);

    List<Routings> updateEntityFromBooking(List<Routings> routingsList, Long bookingId) throws RunnerException;

    List<Routings> saveEntityFromBooking(List<Routings> routings, Long bookingId);
}
