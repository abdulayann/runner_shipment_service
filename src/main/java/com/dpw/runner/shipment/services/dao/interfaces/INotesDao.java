package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.Notes;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Map;
import java.util.Optional;

public interface INotesDao {
    Notes save(Notes notes);
    List<Notes> saveAll(List<Notes> notesList);
    Page<Notes> findAll(Specification<Notes> spec, Pageable pageable);
    Optional<Notes> findById(Long id);
    void delete(Notes notes);
    List<Notes> findByEntityIdAndEntityType(Long entityId, String entityType);
    List<Notes> updateEntityFromOtherEntity(List<Notes> notesList, Long entityId, String entityType) throws RunnerException;
    List<Notes> saveEntityFromOtherEntity(List<Notes> notesRequests, Long entityId, String entityType);
    List<Notes> saveEntityFromOtherEntity(List<Notes> notesRequests, Long entityId, String entityType, Map<Long, Notes> oldEntityMap);
    List<Notes> updateEntityFromOtherEntity(List<Notes> notesList, Long entityId, String entityType, List<Notes> oldEntityList) throws RunnerException;
}
