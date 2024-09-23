package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dao.interfaces.INotesDao;
import com.dpw.runner.shipment.services.entity.Notes;
import com.dpw.runner.shipment.services.entity.enums.LifecycleHooks;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.INotesRepository;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.validator.ValidatorUtility;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;
import java.util.Set;

@Repository
@Slf4j
public class NotesDao implements INotesDao {
    @Autowired
    private INotesRepository notesRepository;
    @Autowired
    private JsonHelper jsonHelper;
    @Autowired
    private IAuditLogService auditLogService;
    @Autowired
    private ValidatorUtility validatorUtility;

    @Override
    public Notes save(Notes notes) {
        Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(notes), Constants.NOTES, LifecycleHooks.ON_CREATE, false);
        if (!errors.isEmpty())
            throw new ValidationException(String.join(",", errors));
        return notesRepository.save(notes);
    }
    @Override
    public List<Notes> saveAll(List<Notes> notesList) {
        for(var notes : notesList){
            Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(notes), Constants.NOTES, LifecycleHooks.ON_CREATE, false);
            if (!errors.isEmpty())
                throw new ValidationException(String.join(",", errors));
        }
        return notesRepository.saveAll(notesList);
    }

    @Override
    public Page<Notes> findAll(Specification<Notes> spec, Pageable pageable) {
        return notesRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<Notes> findById(Long id) {
        return notesRepository.findById(id);
    }

    @Override
    public void delete(Notes notes) {
        notesRepository.delete(notes);
    }

    @Override
    public List<Notes> findByEntityIdAndEntityType(Long entityId, String entityType) {
        return notesRepository.findByEntityIdAndEntityType(entityId, entityType);
    }
}
