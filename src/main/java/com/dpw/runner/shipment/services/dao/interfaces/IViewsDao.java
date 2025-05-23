package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.Views;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import java.util.List;
import java.util.Optional;

public interface IViewsDao {
    Views save(Views views);
    Page<Views> findAll(Specification<Views> spec, Pageable pageable);
    Optional<Views> findById(Long id);
    void delete(Views views);
    List<Views> findAll();
    List<String> findAllByUsernameAndEntity(String username, String entity);
    Optional<Views> findByCreatedByAndEntityAndIsDefault(String createdBy, String entity);
}
