package com.dpw.runner.booking.services.repository.interfaces;

import com.dpw.runner.booking.services.entity.DefaultViews;
import com.dpw.runner.booking.services.utils.Generated;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;
@Generated
//TODO- make this tenant specific as well
public interface IDefaultViewsRepository extends JpaRepository<DefaultViews, Long> {
    Optional<DefaultViews> findByDefaultViewId(Long defaultViewId);
    Optional<DefaultViews> findByUsername(String username);
}
