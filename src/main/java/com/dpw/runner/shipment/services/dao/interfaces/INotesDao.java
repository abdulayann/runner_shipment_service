package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.Notes;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.Optional;

public interface INotesDao {
    Notes save(Notes notes);
    Page<Notes> findAll(Specification<Notes> spec, Pageable pageable);
    Optional<Notes> findById(Long id);
    void delete(Notes notes);
}
