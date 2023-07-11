package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.INotesDao;
import com.dpw.runner.shipment.services.entity.Notes;
import com.dpw.runner.shipment.services.repository.interfaces.INotesRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public class NotesDao implements INotesDao {
    @Autowired
    private INotesRepository notesRepository;

    @Override
    public Notes save(Notes notes) {
        return notesRepository.save(notes);
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
}
