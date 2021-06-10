# The leading dot is a relative path for the whole group of compilation units
module .main;

main(void): void {
	points: Vec<Vec2<u32>> = Vec.new();
	# points for a square, clockwise
	points.push(Vec2.new(0, 0));
	points.push(Vec2.new(0, 32));
	points.push(Vec2.new(32, 32));
	points.push(Vec2.new(32, 0));
	
	for (i in 0..points.len) {
		point: = points[i];
		std.println("point[%ld]: (%d, %d)", i, point->x, point->y);
	}
}

struct Vec: {
	data priv: *$T,
	len: umach,
	cap priv: umach,;
	
	STARTING_CAP: umach = 4;
	
	new(void): self {
		data: = (*$T)alloc.alloc(STARTING_CAP * core.mem.size_of($T));
		if (data == null)
			alloc.oom("failed to allocate for vector");
		ret {
			.data = data,
			.len = 0,
			.cap = STARTING_CAP,
		}
	};
	
	push(*self, item: *$T): void {
		if (self->len == cap->cap)
			self->grow();
		self->data[self->len] = *item;
		self->len += 1;
	};
	
	pop(*self): void {
		if (self->len == 0)
			ret;
		self->len -= 1;
	};
	
	top(*self): *$T {
		if (self->len == 0)
			ret null;
		else
			ret self->data[self->len - 1];
	};
	
	idx(*self, idx: umach): *$T {
		if (self->len < idx)
			ret self->data + idx;
		else
			ret null;
	}
	
	grow(*self) private: void {
		new_size: = self->cap * 2;
		data: = alloc.realloc(self->data, new_size * core.mem.size_of($T));
		if (data == null)
			alloc.oom("failed to reallocate for vector");
		self->data = data;
		self->cap = new_size;
	};
	
	operator[](*self, idx: umach): *$T {
		if (@cfg(debug) == true) {
			if (idx >= self->len)
				std.panic("tried to index at %% where len was %%", idx, self->len);
		}
		
		ret self->data + idx;
	}
	
	_destruct(*self): void {
		alloc.free(self->data);
		self->cap = 0;
		self->len = 0;
	};
};

struct Vec2: {
	x: $T,
	y: $T,;
	
	new(x: $T, y: $T): self {
		tmp: = {.x = x, .y = y};
		ret tmp;
	};
	
	zero(void): self {
		ret {.x = 0, .y = 0};
	};
	
	dot(*self, rhs: *self): $T {
		ret self->x * rhs->x +
		    self->y * rhs->y;
	};
	
	operator+(lhs: *self, rhs: *self): self {
		ret {
			.x = lhs->x + rhs->x,
			.y = lhs->y + rhs->y,
		};
	};
};